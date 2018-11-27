#include "CNode.h"
#include <unistd.h>

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

Napi::Buffer<char> encodePid(erlang_pid* pid, Napi::Env env) {
    ei_x_buff x;
    int size = 0;
    ei_x_new(&x);
    if (ei_x_encode_version(&x) || ei_x_encode_pid(&x, pid)) {
         Napi::Error::New(env, "Could not encode pid").ThrowAsJavaScriptException();
    }
    if (ei_skip_term(x.buff + 1, &size) < 0) {
         printf("ei_skip_term for pid failed, erl_errno: %d\n", erl_errno);
    }
    Napi::Buffer<char> jspid = Napi::Buffer<char>::Copy(env, x.buff, ++size);
    ei_x_free(&x);
    return jspid;
}

/* Waits for receiving a message from a connection */
int ErlangNodeReceive(int fd, erlang_msg* emsg, ei_x_buff* x, int* size) {
  int loop = 1;
  int got;
  while (loop) {
    got = ei_xreceive_msg(fd, emsg, x);
       if (got == ERL_TICK) {
        // ignore
        } else if (got == ERL_ERROR) {
          loop = 0;
          return 0;
        } else {
          if (ei_skip_term(x->buff + 1, size) < 0) {
            printf("ei_skip_term failed, erl_errno: %d\n", erl_errno);
          }
          *size += 1;
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
    status = ErlangNodeReceive(fd, &emsg, &x, &size);
   }

   void OnOK() {
     // Napi::HandleScope scope(Env());
     std::string retCode = "ok";
     if (!status) {
      char buffer[32];
      if (recv(fd, buffer, sizeof(buffer), MSG_PEEK | MSG_DONTWAIT) == 0) {
        // if recv returns zero, that means the connection has been closed.
        retCode = "closed";
      } else retCode = "error";
     }
    // Todo: Make the following code prettier
     Napi::Buffer<char> FromPid;
     if (retCode == "ok" && emsg.msgtype != ERL_SEND) {
      FromPid = encodePid(&emsg.from, Env());
     } else {
      FromPid = Napi::Buffer<char>::New(Env(), 0);
     }
    
    Napi::Buffer<char> ToPid;
     if (retCode == "ok" && emsg.msgtype != ERL_REG_SEND) {
       ToPid = encodePid(&emsg.to, Env());
     } else {
       ToPid = Napi::Buffer<char>::New(Env(), 0);
     }

      if (emsg.msgtype == ERL_REG_SEND) {
     Callback().Call(Env().Null(),  {
      Napi::String::New(Env(), retCode), 
      FromPid, 
      Napi::String::New(Env(), emsg.toname), 
      Napi::Buffer<char>::Copy(Env(), x.buff, size) });   
      } else {
     Callback().Call(Env().Null(),  { 
      Napi::String::New(Env(), retCode), 
      Env().Undefined(), 
      ToPid, 
      Napi::Buffer<char>::Copy(Env(), x.buff, size) });      
      }

     ei_x_free(&x);
   }

 private:
  int fd;
  int size;
  erlang_msg emsg;
  ei_x_buff x;
  int status;
};

class ServerWorker : public Napi::AsyncWorker {
public:
  ServerWorker(Napi::Function& callback, ei_cnode* ec, int sockfd, int pubfd) : Napi::AsyncWorker(callback), 
    ec(ec), sockfd(sockfd), pubfd(pubfd) {}

  ~ServerWorker() {}

  void Execute() {
    fd = ei_accept_tmo(ec, sockfd, &con, 100);
    errornumber = errno;
    erlerrornumber = erl_errno;
  }

  void OnOK() {
    if (fd == ERL_ERROR && errornumber != ECONNABORTED && errornumber != 0) {
      Napi::Error::New(Env(), "Accept failed").ThrowAsJavaScriptException();
    } else if (fd == ERL_ERROR && erlerrornumber == ETIMEDOUT) {
      fd = ERL_TIMEOUT; // -5
    }
    Callback().Call(Env().Null(), { Napi::Number::New(Env(), fd), Napi::String::New(Env(), con.nodename) });
  }

  private:
    ei_cnode* ec;
    int sockfd;
    int pubfd;
    int fd;
    int errornumber;
    int erlerrornumber;
    ErlConnect con;
};

/* Init of a static thing */
int CNode::creation = 0;

/* Some kind of forward reference, needed when wrapping a C++ class */
Napi::FunctionReference CNode::constructor;

/* Napi init. Wraps the class CNode, publishes methods on the wrapped class */
Napi::Object CNode::Init(Napi::Env env, Napi::Object exports) {

  // Required when process works with erl_interface, and presumably also ei
  erl_init(NULL, 0);

  Napi::Function func = DefineClass(env, "CNode", {
    InstanceMethod("connect", &CNode::Connect),
    InstanceMethod("server", &CNode::Server),
    InstanceMethod("accept", &CNode::Accept),
    InstanceMethod("regSend", &CNode::RegSend),
    InstanceMethod("self", &CNode::Self),
    InstanceMethod("unpublish", &CNode::Unpublish)
  });

  constructor = Napi::Persistent(func);
  constructor.SuppressDestruct();

  exports.Set("CNode", func);

  return exports;
}

/* Constructor. Takes a config object with cookie and thisNodeName properties */
CNode::CNode(const Napi::CallbackInfo& info) : Napi::ObjectWrap<CNode>(info)  {
  Napi::Env env = info.Env();
  // Napi::HandleScope scope(env);

  serversocket = -1;

  int length = info.Length();

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
  // Creation should increase only for crashed nodes? Only 2 bits, means 0, 1, 2, or 3.
  int res = ei_connect_init(&einode, &thisNodeName[0], &cookie[0], creation++);
  if (res < 0) {
    Napi::Error::New(env, "Connect init failed").ThrowAsJavaScriptException();
  }
}

CNode::~CNode() {
  if (publishfd > 0) close(publishfd);
  publishfd = -1;
  if (serversocket > 0) close(serversocket);
  serversocket = -1;
}

/* Create a connection to an erlang node using this CNode object. Takes remote node name as parameter */
Napi::Value CNode::Connect(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  if (info.Length() < 1 || !info[0].IsString()) {
    Napi::TypeError::New(env, "Remote node name (string) expected").ThrowAsJavaScriptException();
  }
  std::vector<char> connect = toChar(info[0]);

  int connectionId = CNode::SetUpConnection(env, connect);

  return Napi::Number::New(env, connectionId);
}

Napi::Value CNode::Server(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  int port = -1;

  if (serversocket > 0) {
    Napi::Error::New(env, "Server already started").ThrowAsJavaScriptException();
  } else {

  if (info.Length() < 1 || !info[0].IsNumber()) {
    Napi::TypeError::New(env, "TCP port (integer) expected").ThrowAsJavaScriptException();
  }
  port = info[0].As<Napi::Number>().Int32Value();

  serversocket = socket(AF_INET, SOCK_STREAM, 0);
  if (serversocket < 0)
    Napi::Error::New(env, "ERROR opening socket").ThrowAsJavaScriptException();
 
  struct sockaddr_in serv_addr;
  bzero((char *) &serv_addr, sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   serv_addr.sin_port = htons(port);
   if (bind(serversocket, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
     if (errno == EADDRINUSE) 
      Napi::Error::New(env, "Error port is not available").ThrowAsJavaScriptException();
     else
      Napi::Error::New(env, "ERROR on binding").ThrowAsJavaScriptException();
  }
  
  listen(serversocket,5);

  socklen_t len = sizeof(serv_addr);
  if (getsockname(serversocket, (struct sockaddr *)&serv_addr, &len) == -1) {
    Napi::Error::New(env, "getsockname failed").ThrowAsJavaScriptException();
  }
  port = ntohs(serv_addr.sin_port);

  if ((publishfd = ei_publish(&einode, port)) < 0) {
    Napi::Error::New(env, "Error on publishing server").ThrowAsJavaScriptException();
  }
  stopServer = false;
  }

  return Napi::Number::New(env, port);
}

Napi::Value CNode::Accept(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();

  if (info.Length() < 1 || !info[0].IsFunction()) {
    Napi::TypeError::New(env, "Receive callback function expected").ThrowAsJavaScriptException();
  }
  Napi::Function callback = info[0].As<Napi::Function>();

  if (stopServer) {
    if (publishfd > 0) close(publishfd);
    publishfd = -1;
    if (serversocket > 0) close(serversocket);
    serversocket = -1;
    callback.Call(env.Null(), { Napi::Number::New(env, -1), Napi::String::New(env, "Stopped") });
  }

  if (serversocket > 0) {
    swk = new ServerWorker(callback, &einode, serversocket, publishfd);
    swk->Queue();
  }

  return env.Undefined();
}

Napi::Value CNode::Unpublish(const Napi::CallbackInfo& info) {
  stopServer = true;
  return info.Env().Undefined();
}

/* Send to registered process name */
Napi::Value CNode::RegSend(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();

  if (info.Length() < 1 || !info[0].IsNumber()) {
    Napi::TypeError::New(env, "Connection id (integer) expected").ThrowAsJavaScriptException();
  }
  int fd = info[0].As<Napi::Number>().Int32Value();

  if (info.Length() < 2 || !info[1].IsString()) {
    Napi::TypeError::New(env, "Registered name (string) expected").ThrowAsJavaScriptException();
  }
  std::vector<char> regName = toChar(info[1]);
 
  if (info.Length() < 3 || !info[2].IsBuffer()) {
    Napi::TypeError::New(env, "Buffer expected").ThrowAsJavaScriptException();
  }
  Napi::Buffer<char> buffer = info[2].As<Napi::Buffer<char>>();

  if (ei_reg_send(&einode, fd, &regName[0], buffer.Data(), buffer.Length())) {
    Napi::Error::New(env, "reg send failed").ThrowAsJavaScriptException();
  }

  return env.Undefined();
}

Napi::Value CNode::Self(const Napi::CallbackInfo& info) {
  return encodePid(ei_self(&einode), info.Env());
}

/* Receive (async) a message on a connection. Input is connectionId and callback function */
Napi::Value Receive(const Napi::CallbackInfo& info) {
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
Napi::Value Send(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  if (info.Length() < 1 || !info[0].IsNumber()) {
         Napi::TypeError::New(env, "Connection id (integer) expected").ThrowAsJavaScriptException();
  }
  int fd = info[0].As<Napi::Number>().Int32Value();

  if (info.Length() < 2 || !info[1].IsBuffer()) {
    Napi::TypeError::New(env, "Binary pid expected").ThrowAsJavaScriptException();
  }

  Napi::Buffer<char> topid = info[1].As<Napi::Buffer<char>>();

  erlang_pid epid;
  int index = 0;
  int version;
  if (ei_decode_version(topid.Data(), &index, &version) || ei_decode_pid(topid.Data(), &index, &epid)) {
    Napi::Error::New(env, "Could not decode binary pid").ThrowAsJavaScriptException();
  }

  if (info.Length() < 3 || !info[2].IsBuffer()) {
    Napi::TypeError::New(env, "Buffer expected").ThrowAsJavaScriptException();
  }

  Napi::Buffer<char> buffer = info[2].As<Napi::Buffer<char>>();

  if (ei_send(fd, &epid, buffer.Data(), buffer.Length()) != 0) {
    Napi::Error::New(env, "send failed").ThrowAsJavaScriptException();
  }

  return env.Undefined();
}

Napi::Value Disconnect(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();

  if (info.Length() < 1 || !info[0].IsNumber()) {
    Napi::TypeError::New(env, "connection expected").ThrowAsJavaScriptException();
  }
  int fd = info[0].As<Napi::Number>().Int32Value();

  close(fd);

  return env.Undefined();
}

/* Does the stuff for Connect */
int CNode::SetUpConnection(Napi::Env env, std::vector<char> remoteNode) {
  int fd;
    if ((fd = ei_connect(&einode, &remoteNode[0])) < 0) {
      printf("Connect failed. fd: %d Code: %d\n", fd, erl_errno);
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
