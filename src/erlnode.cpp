#include "erlnode.h"
#include <iostream>
#include <thread>

#include "erl_interface.h"
#include "ei.h"

std::vector<char> toChar(Napi::Value jsString) {
  std::string cppString = jsString.ToString().Utf8Value();
  std::vector<char> cString(cppString.begin(), cppString.end());
  cString.push_back('\0');
  return cString;
}

Napi::FunctionReference ErlNode::constructor;

Napi::Object ErlNode::Init(Napi::Env env, Napi::Object exports) {
  Napi::HandleScope scope(env);

  Napi::Function func = DefineClass(env, "ErlNode", {
    InstanceMethod("receive", &ErlNode::Receive),
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
  int res = erl_connect_init(1, &cookie[0], 0);
  if (!res) {
  Napi::Error::New(env, "Connect init failed").ThrowAsJavaScriptException();
  }

  // connect
  if (config.Has("connect")) {
    if (!config.Get("connect").IsString()) {
      Napi::TypeError::New(env, "connect property string expected").ThrowAsJavaScriptException();
    }
    std::vector<char> connect = toChar(config.Get("connect"));
    printf("Will connect to %s\n", &connect[0]);
    if ((fd = erl_connect(&connect[0])) < 0) {
    Napi::Error::New(env, "Connect failed").ThrowAsJavaScriptException();
  }
  }

  if (config.Has("receiveCallback")) {
  if (!config.Get("receiveCallback").IsFunction()) {
      printf("Will start receive loop thread\n");
      Napi::TypeError::New(env, "receiveCallback property function expected").ThrowAsJavaScriptException();
  }
  receiveCallback = config.Get("receiveCallback").As<Napi::Function>();
  std::thread receiveLoop(&ErlNode::ReceiveLoop, *this, env);

  // receiveLoop.join();
  }
}

void ErlNode::ReceiveLoop(Napi::Env env) {
  printf("Receive loop started\n");
  int loop = 1;
  unsigned char buf[256];
  ErlMessage emsg;
  int got;
  printf("Entering loop, fd: %d\n", fd);
  while (loop) {
    got  = erl_receive_msg(fd, buf, 256, &emsg);
    printf("Got %i\n", got);
       if (got == ERL_TICK) {
          /* ignore */
        } else if (got == ERL_ERROR) {
          loop = 0;
        } else {
        //printf("%s", buf);
        if (ERL_IS_ATOM(emsg.msg)) {
          //cb.Call(env.Global, { Napi::String::New(env, ERL_ATOM_PTR(emsg->msg))});
          receiveCallback.MakeCallback(env.Global(), { Napi::String::New(env, ERL_ATOM_PTR(emsg.msg)) });
        }
      //std::cout << str;
      //std::cout << emsg;
      //loop = 0;
    }
  }
}

Napi::Value ErlNode::Receive(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  // Napi::Function cb = info[0].As<Napi::Function>();
  int loop = 1;
  unsigned char buf[256];
  ErlMessage emsg;
  int got;
  printf("Printf works\n");
  while (loop) {
    got  = erl_receive_msg(fd, buf, 256, &emsg);
    printf("Got %i\n", got);
       if (got == ERL_TICK) {
          /* ignore */
        } else if (got == ERL_ERROR) {
          loop = 0;
        } else {
        //printf("%s", buf);
        if (ERL_IS_ATOM(emsg.msg)) {
          //cb.Call(env.Global, { Napi::String::New(env, ERL_ATOM_PTR(emsg->msg))});
          return Napi::String::New(env, ERL_ATOM_PTR(emsg.msg));
        }
      //std::cout << str;
      //std::cout << emsg;
      loop = 0;
    }
  }
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