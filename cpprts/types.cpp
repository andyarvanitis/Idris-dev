
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
template struct BoxType<'i', int>;

template <>
string BoxType<'i', int>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'i', int>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// BigInt
//---------------------------------------------------------------------------------------
template struct BoxType<'b', long long>;

template <>
string BoxType<'b', long long>::asString() {
  return interpreted_string(value);
}

template <>
long long int BoxType<'b', long long>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Float
//---------------------------------------------------------------------------------------
template struct BoxType<'f', double>;

template <>
string BoxType<'f', double>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'f', double>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// String
//---------------------------------------------------------------------------------------
template struct BoxType<'s', string>;

template <>
string BoxType<'s', string>::asString() {
  return value;
}

template <>
long long int BoxType<'s', string>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Char
//---------------------------------------------------------------------------------------
template struct BoxType<'c', char32_t>;

template <>
string BoxType<'c', char32_t>::asString() {\
  std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> utf32conv;
  string utf8 = utf32conv.to_bytes(value);  
  return interpreted_string(utf8);
}

template <>
long long int BoxType<'c', char32_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word8
//---------------------------------------------------------------------------------------
template struct BoxType<'1', uint8_t>;

template <>
string BoxType<'1', uint8_t>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'1', uint8_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word16
//---------------------------------------------------------------------------------------
template struct BoxType<'2', uint16_t>;

template <>
string BoxType<'2', uint16_t>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'2', uint16_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word32
//---------------------------------------------------------------------------------------
template struct BoxType<'4', uint32_t>;

template <>
string BoxType<'4', uint32_t>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'4', uint32_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// Word64
//---------------------------------------------------------------------------------------
template struct BoxType<'8', uint64_t>;

template <>
string BoxType<'8', uint64_t>::asString() {
  return to_string(value);
}

template <>
long long int BoxType<'8', uint64_t>::asIntegral() {
  return value;
}

//---------------------------------------------------------------------------------------
// ManagedPtr
//---------------------------------------------------------------------------------------
template struct BoxType<'m', shared_ptr<void>>;

template <>
string BoxType<'m', shared_ptr<void>>::asString() {
  return value ? interpreted_string(value.get()) : "nullptr";
}

template <>
long long int BoxType<'m', shared_ptr<void>>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Ptr
//---------------------------------------------------------------------------------------
template struct BoxType<'p', void*>;

template <>
string BoxType<'p', void*>::asString() {
  return interpreted_string(value);
}

template <>
long long int BoxType<'p', void*>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

//---------------------------------------------------------------------------------------
// Con
//---------------------------------------------------------------------------------------
template struct BoxType<'C', Constructor>;

template <>
string BoxType<'C', Constructor>::asString() {
  cannot_convert(this->getTypeId(), "string");
  return "";
}

template <>
long long int BoxType<'C', Constructor>::asIntegral() {
  cannot_convert(this->getTypeId(), "integer");
  return 0;
}

} // namespace idris
