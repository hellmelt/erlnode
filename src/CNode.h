#ifndef ERLNODE_H
#define ERLNODE_H

#include <napi.h>

#ifndef _REENTRANT
#define _REENTRANT /* For some reason __erl_errno is undefined unless _REENTRANT is defined */
#endif

#include "ei.h"
#include "erl_interface.h"

Napi::Value Receive(const Napi::CallbackInfo& info);
Napi::Value Send(const Napi::CallbackInfo& info);
Napi::Value Disconnect(const Napi::CallbackInfo& info);


class CNode : public Napi::ObjectWrap<CNode> {
 public:
  static Napi::Object Init(Napi::Env env, Napi::Object exports);
  CNode(const Napi::CallbackInfo& info);
  ~CNode();

  static int creation;

 private:
  static Napi::FunctionReference constructor;
  int SetUpConnection(Napi::Env, std::vector<char> remoteNode);
  Napi::Value Connect(const Napi::CallbackInfo& info);
  Napi::Value Server(const Napi::CallbackInfo& info);
  Napi::Value Accept(const Napi::CallbackInfo& info);
  Napi::Value RegSend(const Napi::CallbackInfo& info);
  Napi::Value Self(const Napi::CallbackInfo& info);
  Napi::Value Unpublish(const Napi::CallbackInfo& info);

  ei_cnode einode;
  int serversocket;
  int publishfd;
  bool stopServer;
  Napi::AsyncWorker* swk;
};

#endif