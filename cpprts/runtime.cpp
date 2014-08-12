#include <stdio.h>
#include <memory>
#include <functional>
#include <vector>
#include <deque>
#include <stack>
#include <string>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cassert>
#include <sys/utsname.h>

#if __has_feature(cxx_exceptions)
#define RAISE(msg,value) {\
  struct idris_cpp_runtime : std::exception { \
    std::ostringstream message; \
    const char* what() const noexcept { \
      return message.str().c_str(); \
    } \
  }; \
  idris_cpp_runtime exception; \
  exception.message << msg << value; \
  throw exception; \
  }
#else
#define RAISE(msg,value) std::cout << "Idris C++ runtime assertion: " << msg << value << std::endl; assert(false);
#endif

using namespace std;  

using ubigint = unsigned long long int;

struct Constructor;

struct Closure {
  enum class Type {
    Int, UBigInt, Float, String, Con
  };
  
  enum class Op {
    Plus, Minus, Star, Slash, Percent, Equals, Less, LessEquals, Greater, GreaterEquals, ToString
  };
  
  const Type type;
  union {
    int Int;
    ubigint UBigInt;
    double Float;
    string String;
    shared_ptr<Constructor> Con;
  };

  Closure(const int& i) : type(Closure::Type::Int), Int(i) {}
  Closure(const ubigint& i) : type(Closure::Type::UBigInt), UBigInt(i) {}
  Closure(const double& d) : type(Closure::Type::Float), Float(d) {}
  Closure(const string& s) : type(Closure::Type::String), String(s) {}
  Closure(shared_ptr<Constructor> c) : type(Closure::Type::Con), Con(c) {}

  Closure(const Closure& c) : type(c.type) {
    switch (c.type) {
      case Type::Int:
        Int = c.Int;
        break;
      case Type::UBigInt:
        UBigInt = c.UBigInt;
        break;      
      case Type::Float:
        Float = c.Float;
        break;
      case Type::String:
        String = c.String;
        break;
      case Type::Con:
        Con = c.Con;
        break;
    }
  } 

  ~Closure() {}
};

using IndexType = size_t;
using Value = shared_ptr<Closure>;
using Func = void (*)(IndexType,IndexType);

struct Constructor {
  size_t tag;
  vector<Value> args;
  Func function;

  Constructor(const size_t tag, const vector<Value>& args, const Func& function) 
    : tag(tag)
    , args(args)
    , function(function)
    {}
};


//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------

template <typename T>
Value box(T value) {
  return Value(new Closure(value));
}

Value MakeCon(const size_t tag, const vector<Value>& args, const Func& function){
  return box(make_shared<Constructor>(tag,args,function));
}

template <typename T>
T unboxed(const Value& value) {
  RAISE("unknown underlying type when unboxing value of type ", int(value->type));
  return T();
}

template <>
int unboxed(const Value& value) {
  return value->Int;
}

template <>
double unboxed(const Value& value) {
  return value->Float;
}

template <>
string unboxed(const Value& value) {
  return value->String;
}

template <>
ubigint unboxed(const Value& value) {
  return value->UBigInt;
}

string operator-(const string&, const string&) {
  RAISE("'string' does not support '-' operator", "");
  return "";
}

string operator*(const string&, const string&) {
  RAISE("'string' does not support '*' operator", "");
  return "";
}

template <typename T>
Value apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    // Binary
    case Closure::Op::Plus:
      return box(unboxed<T>(lhs) + unboxed<T>(rhs));
    case Closure::Op::Minus:
      return box(unboxed<T>(lhs) - unboxed<T>(rhs));
    case Closure::Op::Star:
      return box(unboxed<T>(lhs) * unboxed<T>(rhs));
    case Closure::Op::Equals:
        return box(unboxed<T>(lhs) == unboxed<T>(rhs));
    case Closure::Op::Less:
      return box(unboxed<T>(lhs) < unboxed<T>(rhs));
    case Closure::Op::LessEquals:
      return box(unboxed<T>(lhs) <= unboxed<T>(rhs));
    // Unary
    case Closure::Op::ToString: {
      ostringstream strstream;
      strstream << unboxed<T>(lhs);
      return box(strstream.str());
    }
    default:
      RAISE("unsupported operator type",int(op));
      return nullptr;
  }
}

Value find_type_and_apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs = nullptr) {
  const Value& operand = lhs ? lhs : rhs;
  switch (operand->type) {
    case Closure::Type::Int:
      return apply_operator<int>(op, lhs, rhs);    
    case Closure::Type::Float:
      return apply_operator<double>(op, lhs, rhs);    
    case Closure::Type::String:
      return apply_operator<string>(op, lhs, rhs);    
    case Closure::Type::UBigInt:
      return apply_operator<ubigint>(op, lhs, rhs);    
    default:
      RAISE("unsupported operand type",int(operand->type));
      return nullptr;
  }
}

Value operator+(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Plus, lhs, rhs);
}

Value operator-(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Minus, lhs, rhs);
}

Value operator*(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Star, lhs, rhs);
}

Value operator==(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Equals, lhs, rhs);
}

Value operator<(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Less, lhs, rhs);
}

Value operator<=(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::LessEquals, lhs, rhs);
}

Value to_string(const Value& value) {
  return find_type_and_apply_operator(Closure::Op::ToString, value);
}

//---------------------------------------------------------------------------------------
class ValueStack : public std::deque<Value> {
public:
  using std::deque<Value>::deque;  
  Value& operator [](size_type index) {
    if (index >= this->size()) {
      this->resize(index + 1, nullptr);
    }
    return this->std::deque<Value>::operator[](index);
  }
};

//---------------------------------------------------------------------------------------
using ArgsPair = pair<IndexType,IndexType>;

using CallStack = stack<Func>;
using ArgStack = stack<ArgsPair>;

//---------------------------------------------------------------------------------------
struct VirtualMachine {
  ValueStack valstack;
  IndexType valstack_top = 0;
  IndexType valstack_base = 0;
  Value ret = nullptr;
  CallStack callstack;
  ArgStack argstack;
};

//---------------------------------------------------------------------------------------
// The current (global) VM
// TODO: change to pointers and references, as appropriate; add thread safety, etc..
//
auto g_vm = make_shared<VirtualMachine>();

//---------------------------------------------------------------------------------------

void schedule(shared_ptr<VirtualMachine>& vm) {
  g_vm = vm;
}

//---------------------------------------------------------------------------------------

void slide(const size_t num_args) {
  for (auto i=0; i < num_args; i++) {
    g_vm->valstack[g_vm->valstack_base + i] = g_vm->valstack[g_vm->valstack_top + i];
  }
}

void project(const Value& value, const IndexType loc, const int arity) {
  assert(value->type == Closure::Type::Con);
  for (auto i=0; i < arity; i++) {
    g_vm->valstack[g_vm->valstack_base + i + loc] = value->Con->args[i];
  }
}

void vmcall(const Func& fn, const ArgsPair& args) {
   g_vm->callstack.push(fn);
   g_vm->argstack.push(args);
}

//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------

template <typename RetType, typename... ArgTypes>
RetType proxy_function(const weak_ptr<VirtualMachine>& vm_weak, const weak_ptr<Closure>& con_weak, const IndexType& oldbase,
                       ArgTypes... args) {  

  void _idris__123_APPLY0_125_(IndexType,IndexType);
  
  // Make sure the vm hasn't been destroyed (nor the function con)
  auto vm = vm_weak.lock();
  auto con = con_weak.lock();
  //
  if (vm and con) {
    // Switch runtime to this vm. TODO: thread safety, etc..
    auto vm_previous = g_vm;
    g_vm = vm;
    
    // Create (empty) private stacks and make them the system's currently used ones.
    CallStack callstack;
    ArgStack argstack;
    g_vm->callstack.swap(callstack);
    g_vm->argstack.swap(argstack);
    
    auto res = con;
    const vector<Closure> arglist = { args... };
    
    for (auto arg : arglist) {
      if (res->type == Closure::Type::Con) {
        g_vm->valstack_top += 1;
        g_vm->valstack[g_vm->valstack_top] = res;
        g_vm->valstack[g_vm->valstack_top + 1] = make_shared<Closure>(arg);
        slide(2);
        g_vm->valstack_top = g_vm->valstack_base + 2;
        vmcall(_idris__123_APPLY0_125_,{oldbase,0});  
        while (g_vm->callstack.size()) {
          auto func = g_vm->callstack.top() ; g_vm->callstack.pop();
          auto fargs = g_vm->argstack.top() ; g_vm->argstack.pop();
          func(get<0>(fargs),get<1>(fargs));
        }  
        res = g_vm->ret;
      }
    }
    
    auto result = g_vm->ret;
    
    // Restore the stacks and vm
    g_vm->callstack.swap(callstack);
    g_vm->argstack.swap(argstack);    
    g_vm = vm_previous;

    return unboxed<RetType>(result);

  } else {
    return RetType(0);
  }
}

//---------------------------------------------------------------------------------------

char charCode(const string& s) {
  return s[0];
}

char fromCharCode(const int cc) {
  return cc;
}

char fromCharCode(const string& s) {
  char c;
  istringstream(s) >> c;
  return c;
}

string systemInfo() {
  ostringstream infoStr;
  infoStr << "C++11 backend";
  utsname info;
  if (uname(&info) > -1) {
    infoStr << " on " << info.sysname;
    infoStr << " "   << info.machine;
  }
  return infoStr.str();
}

//---------------------------------------------------------------------------------------

#define VA_NUM_ARGS(...) VA_NUM_ARGS_(__VA_ARGS__, 10,9,8,7,6,5,4,3,2,1)
#define VA_NUM_ARGS_(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N

#define DISPATCH(func, ...) DISPATCH_(func, VA_NUM_ARGS(__VA_ARGS__))
#define DISPATCH_(func, nargs) DISPATCH__(func, nargs)
#define DISPATCH__(func, nargs) func ## _ ## nargs

#define LAMBDA_CAPTURE_LIST _vm, _fcon, oldbase

#define LAMBDA_WRAPPER(fcon, ...) [&]{ \
  std::weak_ptr<VirtualMachine> _vm = g_vm; \
  std::weak_ptr<Closure> _fcon = fcon; \
  return DISPATCH(LAMBDA_WRAPPER, __VA_ARGS__)(__VA_ARGS__); \
}() // call this lambda right now

#define LAMBDA_WRAPPER_1(R) \
[LAMBDA_CAPTURE_LIST]() -> R { \
  return proxy_function<R>(_vm,_fcon,oldbase); \
}

#define LAMBDA_WRAPPER_2(A1, R) \
[LAMBDA_CAPTURE_LIST](A1 a1) -> R { \
  return proxy_function<R, A1>(_vm,_fcon,oldbase,a1); \
}

#define LAMBDA_WRAPPER_3(A1, A2, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2) -> R { \
  return proxy_function<R, A1, A2>(_vm,_fcon,oldbase,a1,a2); \
}

#define LAMBDA_WRAPPER_4(A1, A2, A3, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3) -> R { \
  return proxy_function<R, A1, A2, A3>(_vm,_fcon,oldbase,a1,a2,a3); \
}

#define LAMBDA_WRAPPER_5(A1, A2, A3, A4, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4) -> R { \
  return proxy_function<R, A1, A2, A3, A4>(_vm,_fcon,oldbase,a1,a2,a3,a4); \
}

#define LAMBDA_WRAPPER_6(A1, A2, A3, A4, A5, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5>(_vm,_fcon,oldbase,a1,a2,a3,a4,a5); \
}

#define LAMBDA_WRAPPER_7(A1, A2, A3, A4, A5, A6, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6>(_vm,_fcon,oldbase,a1,a2,a3,a4,a5,a6); \
}

#define LAMBDA_WRAPPER_8(A1, A2, A3, A4, A5, A6, A7, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7>(_vm,_fcon,oldbase,a1,a2,a3,a4,a5,a6,a7); \
}

#define LAMBDA_WRAPPER_9(A1, A2, A3, A4, A5, A6, A7, A8, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8>(_vm,_fcon,oldbase,a1,a2,a3,a4,a5,a6,a7,a8); \
}

#define LAMBDA_WRAPPER_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8,a9) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8, A9>(_vm,_fcon,oldbase,a1,a2,a3,a4,a5,a6,a7,a8,a9); \
}

//---------------------------------------------------------------------------------------

template <typename T>
T reverse(const T& container) {
  T rcontainer(container);
  std::reverse(rcontainer.begin(),rcontainer.end());
  return rcontainer;
}

//---------------------------------------------------------------------------------------
