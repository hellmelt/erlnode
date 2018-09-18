#include <napi.h>

#include "erlnode.h"

Napi::Object Init(Napi::Env env, Napi::Object exports) {
  return ErlNode::Init(env, exports);
}

NODE_API_MODULE(NODE_GYP_MODULE_NAME, Init)
