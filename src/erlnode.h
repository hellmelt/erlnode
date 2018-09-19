#ifndef ERLNODE_H
#define ERLNODE_H

#include <napi.h>

#ifndef _REENTRANT
#define _REENTRANT /* For some reason __erl_errno is undefined unless _REENTRANT is defined */
#endif

#include "ei.h"
#include "erl_interface.h"


class ErlNode : public Napi::ObjectWrap<ErlNode> {
 public:
  static Napi::Object Init(Napi::Env env, Napi::Object exports);
  ErlNode(const Napi::CallbackInfo& info);

  static int creation_;

 private:
  static Napi::FunctionReference constructor;
  int SetUpConnection(Napi::Env, std::vector<char> remoteNode);
  Napi::Value Connect(const Napi::CallbackInfo& info);
  Napi::Value Receive(const Napi::CallbackInfo& info);
  Napi::Value Send(const Napi::CallbackInfo& info);

  ei_cnode cnode_;
};

#endif