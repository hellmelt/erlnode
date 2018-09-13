#ifndef ERLNODE_H
#define ERLNODE_H

#include <napi.h>

class ErlNode : public Napi::ObjectWrap<ErlNode> {
 public:
  static Napi::Object Init(Napi::Env env, Napi::Object exports);
  ErlNode(const Napi::CallbackInfo& info);
  void ReceiveLoop(Napi::Env env);

 private:
  static Napi::FunctionReference constructor;

  Napi::Value Receive(const Napi::CallbackInfo& info);
  Napi::Value GetValue(const Napi::CallbackInfo& info);
  Napi::Value PlusOne(const Napi::CallbackInfo& info);
  Napi::Value Multiply(const Napi::CallbackInfo& info);


  double value_;

  int fd;

  Napi::Function receiveCallback;
};

#endif