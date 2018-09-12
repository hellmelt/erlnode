#include "erlnode.h"

Napi::FunctionReference ErlNode::constructor;

Napi::Object ErlNode::Init(Napi::Env env, Napi::Object exports) {
  Napi::HandleScope scope(env);

  Napi::Function func = DefineClass(env, "ErlNode", {
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

  if (length <= 0 || !info[0].IsNumber()) {
    Napi::TypeError::New(env, "Number expected").ThrowAsJavaScriptException();
  }

  Napi::Number value = info[0].As<Napi::Number>();
  this->value_ = value.DoubleValue();
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