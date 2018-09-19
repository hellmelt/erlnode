#include <napi.h>

#include "erlnode.h"

Napi::Object Init(Napi::Env env, Napi::Object exports) {
   exports.Set(Napi::String::New(env, "receive"),
              Napi::Function::New(env, Receive));
   exports.Set(Napi::String::New(env, "send"),
              Napi::Function::New(env, Send));
  return ErlNode::Init(env, exports);
}

NODE_API_MODULE(NODE_GYP_MODULE_NAME, Init)
