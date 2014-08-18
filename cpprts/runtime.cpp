#include <stdio.h>
#include <memory>
#include <functional>
#include <vector>
#include <deque>
#include <stack>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
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

struct Closure;
struct Constructor;

using Value = shared_ptr<Closure>;

struct Closure {
  enum class Type {
    Int, BigInt, Float, String, Char, Con,
    Word8, Word16, Word32, Word64,
    ManagedPtr, Ptr
  };
  
  enum class Op {
    Plus, Concat = Plus, Minus, Star, Slash, Percent, 
    Equals, Less, LessEquals, Greater, GreaterEquals, 
    ToString,
    BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight
  };
  
  const Type type;

  union {
    int Int;
    long long int BigInt;
    double Float;
    string String;
    char32_t Char;
    shared_ptr<Constructor> Con;
    uint8_t  Word8;
    uint16_t Word16;
    uint32_t Word32;
    uint64_t Word64;
    shared_ptr<void> ManagedPtr;
    void* Ptr;
  };

  Closure(const Type t) : type(t) {}
  Closure(const string& s) : type(Type::String), String(s) {}
  Closure(const shared_ptr<Constructor>& c) : type(Type::Con), Con(c) {}

  template <typename T>
  static Value Box(T); // { RAISE("unknown box operation type", ""); return nullptr; }

  Closure(const Closure& c) : type(c.type) {
    switch (c.type) {
      case Type::Int:
        Int = c.Int;
        break;
      case Type::BigInt:
        BigInt = c.BigInt;
        break;      
      case Type::Float:
        Float = c.Float;
        break;
      case Type::String:
        String = c.String;
        break;
      case Type::Char:
        Char = c.Char;
        break;
      case Type::Con:
        Con = c.Con;
        break;
      case Type::Word8:
        Word8 = c.Word8;
        break;
      case Type::Word16:
        Word16 = c.Word16;
        break;
      case Type::Word32:
        Word32 = c.Word32;
        break;
      case Type::Word64:
        Word64 = c.Word64;
        break;
      case Type::ManagedPtr:
        ManagedPtr = c.ManagedPtr;
        break;
      case Type::Ptr:
        Ptr = c.Ptr;
        break;
    }
  }

  ~Closure() {}
};

template <>
Value Closure::Box(const int i) {
  auto boxedValue = make_shared<Closure>(Type::Int);
  boxedValue->Int = i;
  return boxedValue;
}

template <>
Value Closure::Box(const long long int i) {
  auto boxedValue = make_shared<Closure>(Type::BigInt);
  boxedValue->BigInt = i;
  return boxedValue;
}

template <>
Value Closure::Box(const unsigned long int i) {
  auto boxedValue = make_shared<Closure>(Type::BigInt);
  boxedValue->BigInt = i;
  return boxedValue;
}

template <>
Value Closure::Box(const double d) {
  auto boxedValue = make_shared<Closure>(Type::Float);
  boxedValue->Float = d;
  return boxedValue;
}

template <>
Value Closure::Box(const string& s) {
  return make_shared<Closure>(s);
}

template <>
Value Closure::Box(const string s) {
  return make_shared<Closure>(s);
}

template <>
Value Closure::Box(char* s) {
  return s ? make_shared<Closure>(s) : nullptr;
}

template <>
Value Closure::Box(const char32_t c) {
  auto boxedValue = make_shared<Closure>(Type::Char);
  boxedValue->Char = c;
  return boxedValue;
}

template <>
Value Closure::Box(const uint8_t w) {
  auto boxedValue = make_shared<Closure>(Type::Word8);
  boxedValue->Word8 = w;
  return boxedValue;
}

template <>
Value Closure::Box(const uint16_t w) {
  auto boxedValue = make_shared<Closure>(Type::Word16);
  boxedValue->Word16 = w;
  return boxedValue;
}

template <>
Value Closure::Box(const uint32_t w) {
  auto boxedValue = make_shared<Closure>(Type::Word32);
  boxedValue->Word32 = w;
  return boxedValue;
}

template <>
Value Closure::Box(const uint64_t w) {
  auto boxedValue = make_shared<Closure>(Type::Word64);
  boxedValue->Word64 = w;
  return boxedValue;
}

template <>
Value Closure::Box(shared_ptr<void> mp) {
  auto boxedValue = make_shared<Closure>(Type::ManagedPtr);
  boxedValue->ManagedPtr = mp;
  return boxedValue;
}

template <>
Value Closure::Box(void* p) {
  auto boxedValue = make_shared<Closure>(Type::Ptr);
  boxedValue->Ptr = p;
  return boxedValue;
}

//---------------------------------------------------------------------------------------

using IndexType = size_t;
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

template <typename T>
Value box(T value) {
  return Closure::Box<T>(value);
}

Value MakeCon(const size_t tag, const vector<Value>& args, const Func& function){
  return make_shared<Closure>(make_shared<Constructor>(tag,args,function));
}

template <typename T>
T unbox(const Value& value) {
  RAISE("unknown underlying type when unboxing value of type ", int(value->type));
  return T();
}

template <>
int unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::Int:
      return value->Int;
    case Closure::Type::BigInt:
      return static_cast<int>(value->BigInt);
    case Closure::Type::Char:
      return value->Char;
    case Closure::Type::String:
      return value->String.front();
    case Closure::Type::Word8:
      return static_cast<int>(value->Word8);
    case Closure::Type::Word16:
      return static_cast<int>(value->Word16);
    case Closure::Type::Word32:
      return static_cast<int>(value->Word32);
    case Closure::Type::Word64:
      return static_cast<int>(value->Word64);
    default:
      RAISE("cannot unbox 'int' from type: ", int(value->type));
      return 0;
  }
}

template <>
double unbox(const Value& value) {
  assert(value->type == Closure::Type::Float);
  return value->Float;
}

template <>
string unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::String:
      return value->String;
    case Closure::Type::Char:
      return string(1,value->Char);
    case Closure::Type::Int:
      return string(1,value->Int);
    case Closure::Type::BigInt:
      return string(1,value->BigInt);
    default:
      RAISE("cannot unbox 'string' from type: ", int(value->type));
      return "";
  }
}

 template <>
 char32_t unbox(const Value& value) {
   switch (value->type) {
    case Closure::Type::Char:
       return value->Char;
    case Closure::Type::String:
       return value->String.front();
    case Closure::Type::Int:
       return value->Int;
    case Closure::Type::BigInt:
       return static_cast<char32_t>(value->BigInt);
    default:
       RAISE("cannot unbox 'char32_t' from type: ", int(value->type));
       return 0;
   }
 }

template <>
long long int unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::BigInt:
      return value->BigInt;
    case Closure::Type::Int:
      return value->Int;
    case Closure::Type::Char:
      return value->Char;
    case Closure::Type::String:
      return value->String.front();
    case Closure::Type::Word8:
      return static_cast<long long int>(value->Word8);
    case Closure::Type::Word16:
      return static_cast<long long int>(value->Word16);
    case Closure::Type::Word32:
      return static_cast<long long int>(value->Word32);
    case Closure::Type::Word64:
      return static_cast<long long int>(value->Word64);
    default:
      RAISE("cannot unbox 'long long int' from type: ", int(value->type));
      return 0;
  }
}


template <>
uint8_t unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word8:
      return value->Word8;
    case Closure::Type::Int:
      return static_cast<uint8_t>(value->Int);
    case Closure::Type::Word16:
      return static_cast<uint8_t>(value->Word16);
    case Closure::Type::Word32:
      return static_cast<uint8_t>(value->Word32);
    case Closure::Type::Word64:
      return static_cast<uint8_t>(value->Word64);
    case Closure::Type::BigInt:
       return static_cast<uint8_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint8_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint16_t unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word16:
      return value->Word16;
    case Closure::Type::Int:
      return static_cast<uint16_t>(value->Int);
    case Closure::Type::Word8:
      return static_cast<uint16_t>(value->Word8);
    case Closure::Type::Word32:
      return static_cast<uint16_t>(value->Word32);
    case Closure::Type::Word64:
      return static_cast<uint16_t>(value->Word64);
    case Closure::Type::BigInt:
       return static_cast<uint16_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint16_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint32_t unbox(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word32:
      return value->Word32;
    case Closure::Type::Int:
      return static_cast<uint32_t>(value->Int);
    case Closure::Type::Word8:
      return static_cast<uint32_t>(value->Word8);
    case Closure::Type::Word16:
      return static_cast<uint32_t>(value->Word16);
    case Closure::Type::Word64:
      return static_cast<uint32_t>(value->Word64);
    case Closure::Type::BigInt:
       return static_cast<uint32_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint32_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint64_t unbox(const Value& value) {
  switch (value->type) {  
    case Closure::Type::Word64:
      return value->Word64;
    case Closure::Type::Int:
      return static_cast<uint64_t>(value->Int);
    case Closure::Type::Word8:
      return static_cast<uint64_t>(value->Word8);
    case Closure::Type::Word16:
      return static_cast<uint64_t>(value->Word16);
    case Closure::Type::Word32:
      return static_cast<uint64_t>(value->Word32);
    case Closure::Type::BigInt:
       return static_cast<uint64_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint64_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
shared_ptr<void> unbox(const Value& value) {
  assert(value->type == Closure::Type::ManagedPtr);
  return value->ManagedPtr;
}

template <>
void* unbox(const Value& value) {
  assert(value->type == Closure::Type::Ptr);
  return value->Ptr;
}


template <typename T>
Value general_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    case Closure::Op::Equals:
      return box<int>(unbox<T>(lhs) == unbox<T>(rhs));
    case Closure::Op::Less:
      return box<int>(unbox<T>(lhs) < unbox<T>(rhs));
    case Closure::Op::LessEquals:
      return box<int>(unbox<T>(lhs) <= unbox<T>(rhs));
    case Closure::Op::Concat:
      return box(unbox<T>(lhs) + unbox<T>(rhs));      
    case Closure::Op::ToString: {
      ostringstream strstream;
      strstream << unbox<T>(lhs);
      return box(strstream.str());
    }
    default:
      RAISE("unsupported operator type",int(op));
      return nullptr;
    }
}

template <typename T>
Value number_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    // Binary
    case Closure::Op::Plus:
      return box(unbox<T>(lhs) + unbox<T>(rhs));
    case Closure::Op::Minus:
      return box(unbox<T>(lhs) - unbox<T>(rhs));
    case Closure::Op::Star:
      return box(unbox<T>(lhs) * unbox<T>(rhs));
    case Closure::Op::Slash:
      return box(unbox<T>(lhs) / unbox<T>(rhs));
    default:
      return general_operator<T>(op, lhs, rhs);
  }
}

template <typename T>
Value integral_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    case Closure::Op::Percent:
      return box(unbox<T>(lhs) % unbox<T>(rhs));
    case Closure::Op::BitAnd:
      return box(unbox<T>(lhs) & unbox<T>(rhs));
    case Closure::Op::BitOr:
      return box(unbox<T>(lhs) | unbox<T>(rhs));
    case Closure::Op::BitXor:
      return box(unbox<T>(lhs) ^ unbox<T>(rhs));
    case Closure::Op::ShiftLeft:
      return box(unbox<T>(lhs) << unbox<T>(rhs));
    case Closure::Op::ShiftRight: {
      return box(unbox<T>(lhs) >> unbox<T>(rhs));
    }
    default:
      return number_operator<T>(op, lhs, rhs);
  }
}

Value find_type_and_apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs = nullptr) {

  switch (lhs->type) {

    case Closure::Type::Int:
      return integral_operator<int>(op, lhs, rhs);
    case Closure::Type::Float:
      return number_operator<double>(op, lhs, rhs);
    case Closure::Type::String:
      return general_operator<string>(op, lhs, rhs);
    case Closure::Type::Char:
      return general_operator<char32_t>(op, lhs, rhs);
    case Closure::Type::BigInt:
      return integral_operator<long long int>(op, lhs, rhs);

    case Closure::Type::Word8:
      return integral_operator<uint8_t>(op,lhs, rhs);
    case Closure::Type::Word16:
      return integral_operator<uint16_t>(op,lhs, rhs);
    case Closure::Type::Word32:
      return integral_operator<uint32_t>(op,lhs, rhs);
    case Closure::Type::Word64:
      return integral_operator<uint64_t>(op,lhs, rhs);

    default:
      RAISE("unsupported operand type",int(lhs->type));
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

Value operator/(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Slash, lhs, rhs);
}

Value operator%(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Percent, lhs, rhs);
}

Value operator==(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Equals, lhs, rhs);
}

Value operator==(const Value& lhs, nullptr_t n) {
  return box<int>(not lhs);
}

Value operator<(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Less, lhs, rhs);
}

Value operator<=(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::LessEquals, lhs, rhs);
}

Value operator&(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitAnd, lhs, rhs);
}

Value operator|(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitOr, lhs, rhs);
}

Value operator^(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitXor, lhs, rhs);
}

Value operator<<(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::ShiftLeft, lhs, rhs);
}

Value operator>>(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::ShiftRight, lhs, rhs);
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
RetType proxy_function(const weak_ptr<VirtualMachine>& vm_weak, 
                       const weak_ptr<Closure>& con_weak, 
                       const IndexType& oldbase,
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

    return unbox<RetType>(result);

  } else {
    return RetType(0);
  }
}

//---------------------------------------------------------------------------------------

Value charCode(const Value& value) {
  switch (value->type) {
    case Closure::Type::Char:
      return box<int>(value->Char);
    case Closure::Type::String:
      return box<int>(value->String.front());
    case Closure::Type::Int:
      return value;
    default:
      RAISE("cannot create char32_t code from type: ", int(value->type));
      return value;
  }
}

Value fromCharCode(const Value& value) {
  switch (value->type) {
    case Closure::Type::Int:
      return box<char32_t>(static_cast<char32_t>(value->Int));
    case Closure::Type::Char:
      return value;
    case Closure::Type::BigInt:
      return box<char32_t>(static_cast<char32_t>(value->BigInt));
    case Closure::Type::Word8:
      return box<char32_t>(static_cast<char32_t>(value->Word8));
    case Closure::Type::Word16:
      return box<char32_t>(static_cast<char32_t>(value->Word16));
    case Closure::Type::Word32:
      return box<char32_t>(static_cast<char32_t>(value->Word32));
    case Closure::Type::Word64:
      return box<char32_t>(static_cast<char32_t>(value->Word64));
    default:
      RAISE("cannot create char32_t from code type: ", int(value->type));
      return value;
  }
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

#define WRAPPER_VARIANT(func, ...) WRAPPER_VARIANT_(func, VA_NUM_ARGS(__VA_ARGS__))
#define WRAPPER_VARIANT_(func, nargs) WRAPPER_VARIANT__(func, nargs)
#define WRAPPER_VARIANT__(func, nargs) func ## _ ## nargs

#define LAMBDA_WRAPPER(fcon, ...) [](weak_ptr<VirtualMachine> _vm, \
                                     weak_ptr<Closure> _fcon, \
                                     const IndexType _oldbase){ \
  return WRAPPER_VARIANT(LAMBDA_WRAPPER, __VA_ARGS__)(__VA_ARGS__); \
}(g_vm, fcon, oldbase) // call this outer lambda right now

#define LAMBDA_CAPTURE_LIST _vm, _fcon, _oldbase

#define LAMBDA_WRAPPER_1(R) \
[LAMBDA_CAPTURE_LIST]() -> R { \
  return proxy_function<R>(LAMBDA_CAPTURE_LIST); \
}

#define LAMBDA_WRAPPER_2(A1, R) \
[LAMBDA_CAPTURE_LIST](A1 a1) -> R { \
  return proxy_function<R, A1>(LAMBDA_CAPTURE_LIST,a1); \
}

#define LAMBDA_WRAPPER_3(A1, A2, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2) -> R { \
  return proxy_function<R, A1, A2>(LAMBDA_CAPTURE_LIST,a1,a2); \
}

#define LAMBDA_WRAPPER_4(A1, A2, A3, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3) -> R { \
  return proxy_function<R, A1, A2, A3>(LAMBDA_CAPTURE_LIST,a1,a2,a3); \
}

#define LAMBDA_WRAPPER_5(A1, A2, A3, A4, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4) -> R { \
  return proxy_function<R, A1, A2, A3, A4>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4); \
}

#define LAMBDA_WRAPPER_6(A1, A2, A3, A4, A5, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5); \
}

#define LAMBDA_WRAPPER_7(A1, A2, A3, A4, A5, A6, R) \
[LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6); \
}

#define LAMBDA_WRAPPER_8(A1, A2, A3, A4, A5, A6, A7, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7); \
}

#define LAMBDA_WRAPPER_9(A1, A2, A3, A4, A5, A6, A7, A8, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7,a8); \
}

#define LAMBDA_WRAPPER_10(A1, A2, A3, A4, A5, A6, A7, A8, A9, R) \
([LAMBDA_CAPTURE_LIST](A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8,a9) -> R { \
  return proxy_function<R, A1, A2, A3, A4, A5, A6, A7, A8, A9>(LAMBDA_CAPTURE_LIST,a1,a2,a3,a4,a5,a6,a7,a8,a9); \
}

//---------------------------------------------------------------------------------------

template <typename T>
T reverse(const T& container) {
  T rcontainer(container);
  std::reverse(rcontainer.begin(),rcontainer.end());
  return rcontainer;
}

//---------------------------------------------------------------------------------------
// C++ versions of the same C runtime functions
//---------------------------------------------------------------------------------------

nullptr_t putStr(const string str) {
  cout << str;
  return nullptr;
}

shared_ptr<void> fileOpen(const string name, const string mode) {
  fstream::openmode openmode = 0x00;
  for (auto flag : mode) {
    switch (flag) {
      case 'r':
        openmode |= fstream::in;
        break;
      case 'w':
        openmode |= fstream::out | fstream::trunc;
        break;
      case 'a':
        openmode |= fstream::out | fstream::app;
        break;
      case 'b':
        openmode |= fstream::binary;
        break;
      case '+':
        openmode |= fstream::out | (openmode & fstream::in ? fstream::app : fstream::in);
        break;
    }
  }
  auto file = make_shared<fstream>();
  file->open(name, openmode);
  return file->fail() ? nullptr : file;
}

nullptr_t fileClose(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  file->close();
  return nullptr;
}

string freadStr(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  string str;
  getline(*file,str);
  if (not file->eof()) {
    str += '\n';
  }
  return str;
}

nullptr_t fputStr(shared_ptr<void> h, const string str) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  *file << str;
  return nullptr;
}

int fileEOF(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  return file->eof();
}

int fileError(shared_ptr<void> h) {
  assert(h);
  auto file = static_pointer_cast<fstream>(h);
  return file->fail();
}

//---------------------------------------------------------------------------------------
