#ifndef __idris_cpp_runtime_h_
#define __idris_cpp_runtime_h_

#include <memory>
#include <vector>
#include <string>
#include <functional>
#include "exceptions.h"

namespace idris {

using namespace std;  

struct Closure;
struct Constructor;

//---------------------------------------------------------------------------------------

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



} // namespace idris


#endif // __idris_cpp_runtime_h_

