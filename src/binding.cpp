#include <napi.h>

#include "CNode.h"

Napi::Object Init(Napi::Env env, Napi::Object exports) {
   exports.Set(Napi::String::New(env, "receive"),
              Napi::Function::New(env, Receive));
   exports.Set(Napi::String::New(env, "send"),
              Napi::Function::New(env, Send));
   exports.Set(Napi::String::New(env, "disconnect"),
              Napi::Function::New(env, Disconnect));
  return CNode::Init(env, exports);
}

NODE_API_MODULE(NODE_GYP_MODULE_NAME, Init)
