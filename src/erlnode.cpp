#include "erlnode.h"  
#include <iostream>
#include <thread>

//#include "erl_interface.h"
//#include "ei.h"

std::vector<char> toChar(Napi::Value jsString) {
  std::string cppString = jsString.ToString().Utf8Value();
  std::vector<char> cString(cppString.begin(), cppString.end());
  cString.push_back('\0');
  return cString;
}

// Napi::Buffer<char> ErlangNodeReceive(Napi::Env env, int fd) {
char* ErlangNodeReceive(int fd, int* size) {
  printf("ErlangNodeRecicve\n"); 
  int loop = 1;
  erlang_msg emsg;
  int got;
  ei_x_buff x;
  ei_x_new(&x);
  printf("ei_x_buff allocated\n");
  while (loop) {
    got = ei_receive_msg(fd, &emsg, &x);
//    got  = erl_receive_msg(fd, buf, 256, &emsg);
    printf("Got %d\n", got);
       if (got == ERL_TICK) {
        // ignore
        } else if (got == ERL_ERROR) {
          loop = 0;
        } else {
                  printf("Received: %s\nSize %d Index %d\n", x.buff, x.buffsz, x.index);
                  printf("Buffer 24 first chars:\n");
                  for (int i = 0; i < 24; i++) printf("%d ", x.buff[i]);
                  printf("\nVersion, tag, len: %d %d %d\n", x.buff[0], x.buff[1], x.buff[2]);
                  printf("just before skip_term\n");
                  if (ei_skip_term(x.buff, size) < 0) {
                    printf("ei_skip_term failed, erl_errno: %d\n", erl_errno);
                  }
                  printf("Msg size: %d\n", *size);
                  return x.buff;
                  // return Napi::Buffer<char>::Copy(env, x.buff, msg_size);
       
      loop = 0;
    }
  }
}

class ReceiveWorker : public Napi::AsyncWorker {
public: 
  ReceiveWorker(Napi::Function& callback, int fd) : Napi::AsyncWorker(callback), fd(fd) {}

   ~ReceiveWorker() {}

   void Execute() {
    printf("File desc: %d\n", fd);
    pBuf = ErlangNodeReceive(fd, &size);
    printf("Async Execute complete\n");
   }

   void OnOk() {
     printf("OnOK, buffer start: %d %d %d\n", pBuf[0], pBuf[1], pBuf[2]);
     Napi::HandleScope scope(Env());
     Callback().Call({Env().Null(),  Napi::Buffer<char>::Copy(Env(), pBuf, size)});
   }

 private:
  int fd;
  char* pBuf;
  int size;
};


Napi::FunctionReference ErlNode::constructor;

Napi::Object ErlNode::Init(Napi::Env env, Napi::Object exports) {
  Napi::HandleScope scope(env);

  Napi::Function func = DefineClass(env, "ErlNode", {
    InstanceMethod("receive", &ErlNode::Receive),
    InstanceMethod("receiveAsync", &ErlNode::ReceiveAsync),
    InstanceMethod("plusOne", &ErlNode::PlusOne),
    InstanceMethod("value", &ErlNode::GetValue),
    InstanceMethod("multiply", &ErlNode::Multiply)
  });

  constructor = Napi::Persistent(func);
  constructor.SuppressDestruct();

  exports.Set("ErlNode", func);
  return exports;
}

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

  // connect_init
  int res = ei_connect_init(&cnode_, "jsnode", &cookie[0], creation_++);
  // int res = erl_connect_init(1, &cookie[0], 0);
  if (res < 0) {
  Napi::Error::New(env, "Connect init failed").ThrowAsJavaScriptException();
  }

  // connect
  if (config.Has("connect")) {
    if (!config.Get("connect").IsString()) {
      Napi::TypeError::New(env, "connect property string expected").ThrowAsJavaScriptException();
    }
    std::vector<char> connect = toChar(config.Get("connect"));
    printf("Will connect to %s\n", &connect[0]);
    if ((fd = ei_connect(&cnode_, &connect[0])) < 0) {
    // if ((fd = erl_connect(&connect[0])) < 0) {
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
  }

  if (config.Has("receiveCallback")) {
  if (!config.Get("receiveCallback").IsFunction()) {
      Napi::TypeError::New(env, "receiveCallback property function expected").ThrowAsJavaScriptException();
  }
  Napi::Function receiveCallback = config.Get("receiveCallback").As<Napi::Function>();
      printf("Will start receive loop thread\n");

  ReceiveWorker* wk = new ReceiveWorker(receiveCallback, fd);
  wk->Queue();
  // std::thread receiveLoop(&ErlNode::ReceiveLoop, *this, env);
  // receiveCallback.Call(1, ErlangNodeReceive(env, fd));
  }
}

Napi::Value ErlNode::Receive(const Napi::CallbackInfo& info) {
  // return ErlangNodeReceive(info.Env(), fd);
  int size;
  char * pBuf = ErlangNodeReceive(fd, &size);
  //                 int msg_size;
  //                 printf("Magic, tag, len: %d, %d, %d\n", pBuf[0], pBuf[1], pBuf[2]);
  //                 ei_skip_term(&(pBuf[1]), &msg_size);
  //                 printf("Msg size: %d\n", msg_size);
  return Napi::Buffer<char>::Copy(info.Env(), pBuf, size);
}

Napi::Value ErlNode::ReceiveAsync(const Napi::CallbackInfo& info) {
  if (info.Length() < 1 && !info[0].IsFunction()) {
    Napi::Error::New(info.Env(), "Function expected").ThrowAsJavaScriptException();
  }
  Napi::Function receiveCallback = info[0].As<Napi::Function>();
  ReceiveWorker* wk = new ReceiveWorker(receiveCallback, fd);
  wk->Queue();
  return info.Env().Undefined();
}

Napi::Value ErlNode::GetValue(const Napi::CallbackInfo& info) {
  double num = this->value_;

  return Napi::Number::New(info.Env(), num);
}

Napi::Value ErlNode::PlusOne(const Napi::CallbackInfo& info) {
  this->value_ = this->value_ + 1;

  return ErlNode::GetValue(info);
}

Napi::Value ErlNode::Multiply(const Napi::CallbackInfo& info) {
  Napi::Number multiple;
  if (info.Length() <= 0 || !info[0].IsNumber()) {
    multiple = Napi::Number::New(info.Env(), 1);
  } else {
    multiple = info[0].As<Napi::Number>();
  }

  Napi::Object obj = constructor.New({ Napi::Number::New(info.Env(), this->value_ * multiple.DoubleValue()) });

  return obj;
}