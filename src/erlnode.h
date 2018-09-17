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
  void ReceiveLoop(Napi::Env env);

 private:
  static Napi::FunctionReference constructor;

  Napi::Value Receive(const Napi::CallbackInfo& info);
  Napi::Value ReceiveAsync(const Napi::CallbackInfo& info);
  Napi::Value GetValue(const Napi::CallbackInfo& info);
  Napi::Value PlusOne(const Napi::CallbackInfo& info);
  Napi::Value Multiply(const Napi::CallbackInfo& info);


  double value_;

  int fd;

  Napi::Function receiveCallback;

  ei_cnode cnode_;
  int creation_ = 0;
};

#endif