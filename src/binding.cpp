#include <napi.h>

#include "erlnode.h"

#include "erl_interface.h"
#include "ei.h"

//using namespace Napi;
Napi::Value Hej(const Napi::CallbackInfo& info) {
    Napi::Env env = info.Env();
    return Napi::String::New(env, "Hej du!");
}

Napi::Value erlConnectInit(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  int number = info[0].ToNumber().Int32Value();
  std::string cookie = info[1].ToString().Utf8Value();
  char * pCookie = new char[cookie.size() + 1];
  std::copy(cookie.begin(), cookie.end(), pCookie);
  pCookie[cookie.size()] = '\0';
  short creation = info[2].ToNumber().Int32Value();
  int res = erl_connect_init(number, pCookie, creation);
  return Napi::Number::New(env, (double)res);
}

Napi::Value erlConnect(const Napi::CallbackInfo& info) {
  int fd;
  Napi::Env env = info.Env();
  std::string node = info[0].ToString().Utf8Value();
  char * pNode = new char[node.size() + 1];
    std::copy(node.begin(), node.end(), pNode);
    pNode[node.size()] = '\0';
     if ((fd = erl_connect(pNode)) < 0)
        erl_err_quit("erl_connect failed");
      fprintf(stderr, "Connected to ei@idril\n\r");
}

Napi::Object Init(Napi::Env env, Napi::Object exports) {
  erl_init(NULL, 0);
  exports.Set(Napi::String::New(env, "hej"),
              Napi::Function::New(env, Hej));
  exports.Set(Napi::String::New(env, "erlConnectInit"),
              Napi::Function::New(env, erlConnectInit));
  exports.Set(Napi::String::New(env, "erlConnect"),
              Napi::Function::New(env, erlConnect));
  return ErlNode::Init(env, exports);
}

NODE_API_MODULE(NODE_GYP_MODULE_NAME, Init)
