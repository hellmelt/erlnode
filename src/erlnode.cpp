#include "erlnode.h"  

/* Utility to get something compatible to char *, does not need explicit free */
std::vector<char> toChar(Napi::Value jsString) {
  std::string cppString = jsString.ToString().Utf8Value();
  std::vector<char> cString(cppString.begin(), cppString.end());
  cString.push_back('\0');
  return cString;
}

void print_pid(char* label, erlang_pid* pid) {
  printf("%s: node %s, num %d, serial %d, creation %d\n", label, pid->node, pid->num, pid->serial, pid->creation);
}

/* Waits for receiving a message from a connection */
int ErlangNodeReceive(int fd, erlang_msg* emsg, ei_x_buff* x, int* size) {
  int loop = 1;
  int got;
  printf("ei_x_buff allocated\n");
  while (loop) {
    got = ei_xreceive_msg(fd, emsg, x);
    printf("Got %d\n", got);
       if (got == ERL_TICK) {
        // ignore
        } else if (got == ERL_ERROR) {
          loop = 0;
          return 0;
        } else {
          printf("Received: %s\nSize %d Index %d\n", x->buff, x->buffsz, x->index);
          printf("Buffer 24 first chars:\n");
          for (int i = 0; i < 24; i++) printf("%d ", x->buff[i]);
            printf("\nVersion, tag, len: %d %d %d\n", x->buff[0], x->buff[1], x->buff[2]);
          printf("just before skip_term\n");
          if (ei_skip_term(x->buff + 1, size) < 0) {
            printf("ei_skip_term failed, erl_errno: %d\n", erl_errno);
          }
          printf("emsg: type %ld \n", emsg->msgtype);
          print_pid("Pid from", &emsg->from);
          print_pid("Pid to", &emsg->to);
          printf("toname: %s\n", &emsg->toname);
          printf("cookie: %s\n", &emsg->cookie);
          *size += 1;
          printf("Msg size: %d\n", *size);
          loop = 0;
    }
  }
  return 1;
}

/* A class like this is the way to do async stuff in Napi.
 * This one waits (async) for a message from a connection */
class ReceiveWorker : public Napi::AsyncWorker {
public: 
  ReceiveWorker(Napi::Function& callback, int fd) : Napi::AsyncWorker(callback), fd(fd) {}

   ~ReceiveWorker() {}

   void Execute() {
    size = 0;
    ei_x_new(&x);
    printf("File desc: %d\n", fd);
    status = ErlangNodeReceive(fd, &emsg, &x, &size);
    printf("Async Execute complete\n");
   }

   void OnOK() {
     printf("OnOK, buffer start: %d %d %d\n", x.buff[0], x.buff[1], x.buff[2]);
     Napi::HandleScope scope(Env());
     if (!status) {
       Napi::Error::New(Env(), "Async receive failed").ThrowAsJavaScriptException();
     }
     Napi::Object FromPid = Napi::Object::New(Env());
     FromPid.Set("node", emsg.from.node);
     FromPid.Set("num", emsg.from.num);
     FromPid.Set("serial", emsg.from.serial);
     FromPid.Set("creation", emsg.from.creation);
     Callback().Call(Env().Null(),  { FromPid, Napi::String::New(Env(), emsg.toname), Napi::Buffer<char>::Copy(Env(), x.buff, size) });
     ei_x_free(&x);
   }

 private:
  int fd;
  int size;
  erlang_msg emsg;
  ei_x_buff x;
  int status;
};

/* Init of a static thing */
int ErlNode::creation_ = 0;

/* Some kind of forward reference, needed when wrapping a C++ class */
Napi::FunctionReference ErlNode::constructor;

/* Napi init. Wraps the class ErlNode, publishes methods on the wrapped class */
Napi::Object ErlNode::Init(Napi::Env env, Napi::Object exports) {

  // Required when process works with erl_interface, and presumably also ei
  erl_init(NULL, 0);

  Napi::HandleScope scope(env);

  Napi::Function func = DefineClass(env, "ErlNode", {
    InstanceMethod("connect", &ErlNode::Connect),
    InstanceMethod("receive", &ErlNode::Receive),
    InstanceMethod("send", &ErlNode::Send)
  });

  constructor = Napi::Persistent(func);
  constructor.SuppressDestruct();

  exports.Set("ErlNode", func);

  return exports;
}

/* Constructor. Takes a config object with cookie and thisNodeName properties */
ErlNode::ErlNode(const Napi::CallbackInfo& info) : Napi::ObjectWrap<ErlNode>(info)  {
  Napi::Env env = info.Env();
  Napi::HandleScope scope(env);

  int length = info.Length();

  // Remote node
  if (length <= 0 || !info[0].IsObject()) {
    Napi::TypeError::New(env, "Object expected").ThrowAsJavaScriptException();
  }
  Napi::Object config = info[0].As<Napi::Object>();
  if (!config.Has("cookie") || !config.Get("cookie").IsString()) {
    Napi::TypeError::New(env, "cookie string property expected").ThrowAsJavaScriptException();
  }
  std::vector<char> cookie = toChar(config.Get("cookie"));

  if (!config.Has("thisNodeName") || !config.Get("thisNodeName").IsString()) {
    Napi::TypeError::New(env, "thisNodeName string property expected").ThrowAsJavaScriptException();
  }
  std::vector<char> thisNodeName = toChar(config.Get("thisNodeName"));

  // connect_init
  int res = ei_connect_init(&cnode_, &thisNodeName[0], &cookie[0], creation_++);
  if (res < 0) {
    Napi::Error::New(env, "Connect init failed").ThrowAsJavaScriptException();
  }
}

/* Create a connection to an erlang node using this ErlNode object. Takes remote node name as parameter */
Napi::Value ErlNode::Connect(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  if (info.Length() < 1 || !info[0].IsString()) {
    Napi::TypeError::New(env, "Remote node name (string) expected").ThrowAsJavaScriptException();
  }
  std::vector<char> connect = toChar(info[0]);

  int connectionId = ErlNode::SetUpConnection(env, connect);

  return Napi::Number::New(env, connectionId);
}

/* Receive (async) a message on a connection. Input is connectionId and callback function */
Napi::Value ErlNode::Receive(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  if (info.Length() < 1 || !info[0].IsNumber()) {
         Napi::TypeError::New(env, "Connection id (integer) expected").ThrowAsJavaScriptException();
  }
  int fd = info[0].As<Napi::Number>().Int32Value();

  if (info.Length() < 2 || !info[1].IsFunction()) {
    Napi::TypeError::New(env, "Receive callback function expected").ThrowAsJavaScriptException();
  }
  Napi::Function callback = info[1].As<Napi::Function>();

  ReceiveWorker* wk = new ReceiveWorker(callback, fd);
  wk->Queue();

  return env.Undefined();
}

/* Send a message on a connection. Input is connectionId and message (an encoded term) */
Napi::Value ErlNode::Send(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  if (info.Length() < 1 || !info[0].IsNumber()) {
         Napi::TypeError::New(env, "Connection id (integer) expected").ThrowAsJavaScriptException();
  }
  int fd = info[0].As<Napi::Number>().Int32Value();

  if (info.Length() < 2 || !info[1].IsObject()) {
    Napi::TypeError::New(env, "Pid object expected").ThrowAsJavaScriptException();
  }

  Napi::Object pid = info[1].As<Napi::Object>();

  erlang_pid epid;

  if (!pid.Has("node") || !pid.Get("node").IsString()) {
    Napi::TypeError::New(env, "node string property expected in pid object").ThrowAsJavaScriptException();
  }
  std::vector<char> node = toChar(pid.Get("node"));
  strncpy(epid.node, &node[0], node.size() < MAXATOMLEN ? node.size() : MAXATOMLEN);

  if (!pid.Has("num") || !pid.Get("num").IsNumber()) {
    Napi::TypeError::New(env, "num number property expected in pid object").ThrowAsJavaScriptException();
  }
  int num = pid.Get("num").As<Napi::Number>().Int32Value();

  if (!pid.Has("serial") || !pid.Get("serial").IsNumber()) {
    Napi::TypeError::New(env, "serial number property expected in pid object").ThrowAsJavaScriptException();
  }
  int serial = pid.Get("serial").As<Napi::Number>().Int32Value();

  if (!pid.Has("creation") || !pid.Get("creation").IsNumber()) {
    Napi::TypeError::New(env, "creation number property expected in pid object").ThrowAsJavaScriptException();
  }
  int creation = pid.Get("creation").As<Napi::Number>().Int32Value();

  epid.num = num;
  epid.serial = serial;
  epid.creation = creation;

  if (info.Length() < 3 || !info[2].IsBuffer()) {
    Napi::TypeError::New(env, "Buffer expected").ThrowAsJavaScriptException();
  }
  Napi::Buffer<char> buffer = info[2].As<Napi::Buffer<char>>();

  if (!ei_send(fd, &epid, buffer.Data(), buffer.Length()));

  return env.Undefined();
}

/* Does the stuff for Connect */
int ErlNode::SetUpConnection(Napi::Env env, std::vector<char> remoteNode) {
  int fd;
   printf("Will connect to %s\n", &remoteNode[0]);
    if ((fd = ei_connect(&cnode_, &remoteNode[0])) < 0) {
      if (erl_errno == EHOSTUNREACH) {
         Napi::Error::New(env, "The remote node is unreachable").ThrowAsJavaScriptException();
         }
       if (erl_errno == ENOMEM) {
         Napi::Error::New(env, "No more memory is available").ThrowAsJavaScriptException();
         }
       if (erl_errno == EIO) {
         Napi::Error::New(env, "I/O Error").ThrowAsJavaScriptException();
       }
       Napi::Error::New(env, "Connect failed").ThrowAsJavaScriptException();
     }

     return fd;
}
