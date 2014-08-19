
#include "boxing.h"

namespace idris {

using namespace std;  

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
Value Closure::Box(const char* s) {
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


} // namespace idris
