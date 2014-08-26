
#include <sstream>
#include <iostream>
#include <codecvt>
#include "box.h"

namespace idris {
  
using namespace std;

template <typename T>
string interpreted_string(T value) {
  ostringstream strstream;
  strstream << value;
  return strstream.str();
}

void cannot_convert(char src, string tgt) {
  cout << "Cannot convert type " << src << " to " << tgt << "!" << endl;
}


//---------------------------------------------------------------------------------------
// Int
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'i', int>;

template <>
string TypedBoxedValue<'i', int>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'i', int>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// BigInt
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'b', long long>;

template <>
string TypedBoxedValue<'b', long long>::asString() {
  return interpreted_string(value);
}

template <>
long long int TypedBoxedValue<'b', long long>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Float
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'f', double>;

template <>
string TypedBoxedValue<'f', double>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'f', double>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// String
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'s', string>;

template <>
string TypedBoxedValue<'s', string>::asString() {
  return value;
}

template <>
long long int TypedBoxedValue<'s', string>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Char
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'c', char32_t>;

template <>
string TypedBoxedValue<'c', char32_t>::asString() {\
  std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> utf32conv;
  string utf8 = utf32conv.to_bytes(value);  
  return interpreted_string(utf8);
}

template <>
long long int TypedBoxedValue<'c', char32_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word8
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'1', uint8_t>;

template <>
string TypedBoxedValue<'1', uint8_t>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'1', uint8_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word16
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'2', uint16_t>;

template <>
string TypedBoxedValue<'2', uint16_t>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'2', uint16_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word32
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'4', uint32_t>;

template <>
string TypedBoxedValue<'4', uint32_t>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'4', uint32_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word64
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'8', uint64_t>;

template <>
string TypedBoxedValue<'8', uint64_t>::asString() {
  return to_string(value);
}

template <>
long long int TypedBoxedValue<'8', uint64_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// ManagedPtr
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'m', shared_ptr<void>>;

template <>
string TypedBoxedValue<'m', shared_ptr<void>>::asString() {
  return value ? interpreted_string(value.get()) : "nullptr";
}

template <>
long long int TypedBoxedValue<'m', shared_ptr<void>>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Ptr
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'p', void*>;

template <>
string TypedBoxedValue<'p', void*>::asString() {
  return interpreted_string(value);
}

template <>
long long int TypedBoxedValue<'p', void*>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Con
//---------------------------------------------------------------------------------------
template struct TypedBoxedValue<'C', Constructor>;

template <>
string TypedBoxedValue<'C', Constructor>::asString() {
  cannot_convert(this->getTypeId(), "string");
  return "";
}

template <>
long long int TypedBoxedValue<'C', Constructor>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

} // namespace idris
