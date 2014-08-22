
#include "boxing_base.h"

namespace idris {

using namespace std;  

template <>
void init_closure<Closure::Type::Int>(Value& closure, int i) {
  closure->Int = i;
}

template <>
void init_closure<Closure::Type::BigInt>(Value& closure, long long int i) {
  closure->BigInt = i;
}

// template <>
// void init_closure<Closure::Type::BigInt>(Value& closure, unsigned long int i) {
//   closure->BigInt = i;
// }

template <>
void init_closure<Closure::Type::Float>(Value& closure, double d) {
  closure->Float = d;
}

template <>
Value box<Closure::Type::String>(string s) {
  return make_shared<Closure>(s);
}

template <>
Value box<Closure::Type::String>(char* s) {
  return s ? make_shared<Closure>(s) : nullptr;
}

template <>
Value box<Closure::Type::String>(const char* s) {
  return s ? make_shared<Closure>(s) : nullptr;
}

template <>
void init_closure<Closure::Type::Char>(Value& closure, char32_t c) {
  closure->Char = c;
}

template <>
void init_closure<Closure::Type::Word8>(Value& closure, uint8_t w) {
  closure->Word8 = w;
}

template <>
void init_closure<Closure::Type::Word16>(Value& closure, uint16_t w) {
  closure->Word16 = w;
}

template <>
void init_closure<Closure::Type::Word32>(Value& closure, uint32_t w) {
  closure->Word32 = w;
}

template <>
void init_closure<Closure::Type::Word64>(Value& closure, uint64_t w) {
  closure->Word64 = w;
}

template <>
Value box<Closure::Type::ManagedPtr>(shared_ptr<void> mp) {
  return make_shared<Closure>(mp); 
}

template <>
void init_closure<Closure::Type::Ptr>(Value& closure, void* p) {
  closure->Ptr = p;
}

template <>
int unbox<Closure::Type::Int>(const Value& value) {
  switch (value->type) {
    case Closure::Type::Int:
      return value->Int;
    // case Closure::Type::BigInt:
    //   return static_cast<int>(value->BigInt);
    // case Closure::Type::Char:
    //   return value->Char;
    // case Closure::Type::String:
    //   return value->String.front();
    // case Closure::Type::Word8:
    //   return static_cast<int>(value->Word8);
    // case Closure::Type::Word16:
    //   return static_cast<int>(value->Word16);
    // case Closure::Type::Word32:
    //   return static_cast<int>(value->Word32);
    // case Closure::Type::Word64:
    //   return static_cast<int>(value->Word64);
    default:
      RAISE("cannot unbox 'int' from type: ", int(value->type));
      return 0;
  }
}

template <>
double unbox<Closure::Type::Float>(const Value& value) {
  assert(value->type == Closure::Type::Float);
  return value->Float;
}

template <>
string unbox<Closure::Type::String>(const Value& value) {
  switch (value->type) {
    case Closure::Type::String:
      return value->String;
    // case Closure::Type::Char:
    //   return string(1,value->Char);
    // case Closure::Type::Int:
    //   return string(1,value->Int);
    // case Closure::Type::BigInt:
    //   return string(1,value->BigInt);
    default:
      RAISE("cannot unbox 'string' from type: ", int(value->type));
      return "";
  }
}

template <>
char32_t unbox<Closure::Type::Char>(const Value& value) {
   switch (value->type) {
    case Closure::Type::Char:
       return value->Char;
    // case Closure::Type::String:
    //    return value->String.front();
    // case Closure::Type::Int:
    //    return value->Int;
    // case Closure::Type::BigInt:
    //    return static_cast<char32_t>(value->BigInt);
    default:
       RAISE("cannot unbox 'char32_t' from type: ", int(value->type));
       return 0;
   }
 }

template <>
long long int unbox<Closure::Type::BigInt>(const Value& value) {
  switch (value->type) {
    case Closure::Type::BigInt:
      return value->BigInt;
    // case Closure::Type::Int:
    //   return value->Int;
    // case Closure::Type::Char:
    //   return value->Char;
    // case Closure::Type::String:
    //   return value->String.front();
    // case Closure::Type::Word8:
    //   return static_cast<long long int>(value->Word8);
    // case Closure::Type::Word16:
    //   return static_cast<long long int>(value->Word16);
    // case Closure::Type::Word32:
    //   return static_cast<long long int>(value->Word32);
    // case Closure::Type::Word64:
    //   return static_cast<long long int>(value->Word64);
    default:
      RAISE("cannot unbox 'long long int' from type: ", int(value->type));
      return 0;
  }
}


template <>
uint8_t unbox<Closure::Type::Word8>(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word8:
      return value->Word8;
    // case Closure::Type::Int:
    //   return static_cast<uint8_t>(value->Int);
    // case Closure::Type::Word16:
    //   return static_cast<uint8_t>(value->Word16);
    // case Closure::Type::Word32:
    //   return static_cast<uint8_t>(value->Word32);
    // case Closure::Type::Word64:
    //   return static_cast<uint8_t>(value->Word64);
    // case Closure::Type::BigInt:
    //    return static_cast<uint8_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint8_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint16_t unbox<Closure::Type::Word16>(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word16:
      return value->Word16;
    // case Closure::Type::Int:
    //   return static_cast<uint16_t>(value->Int);
    // case Closure::Type::Word8:
    //   return static_cast<uint16_t>(value->Word8);
    // case Closure::Type::Word32:
    //   return static_cast<uint16_t>(value->Word32);
    // case Closure::Type::Word64:
    //   return static_cast<uint16_t>(value->Word64);
    // case Closure::Type::BigInt:
    //    return static_cast<uint16_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint16_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint32_t unbox<Closure::Type::Word32>(const Value& value) {
  switch (value->type) {
    case Closure::Type::Word32:
      return value->Word32;
    // case Closure::Type::Int:
    //   return static_cast<uint32_t>(value->Int);
    // case Closure::Type::Word8:
    //   return static_cast<uint32_t>(value->Word8);
    // case Closure::Type::Word16:
    //   return static_cast<uint32_t>(value->Word16);
    // case Closure::Type::Word64:
    //   return static_cast<uint32_t>(value->Word64);
    // case Closure::Type::BigInt:
    //    return static_cast<uint32_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint32_t' from type: ", int(value->type));
      return 0;
  }
}

template <>
uint64_t unbox<Closure::Type::Word64>(const Value& value) {
  switch (value->type) {  
    case Closure::Type::Word64:
      return value->Word64;
    // case Closure::Type::Int:
    //   return static_cast<uint64_t>(value->Int);
    // case Closure::Type::Word8:
    //   return static_cast<uint64_t>(value->Word8);
    // case Closure::Type::Word16:
    //   return static_cast<uint64_t>(value->Word16);
    // case Closure::Type::Word32:
    //   return static_cast<uint64_t>(value->Word32);
    // case Closure::Type::BigInt:
    //    return static_cast<uint64_t>(value->BigInt);
    default:
      RAISE("cannot unbox 'uint64_t' from type: ", int(value->type));
      return 0;
  }
}

} // namespace idris
