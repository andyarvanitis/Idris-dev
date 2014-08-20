#include "utility.h"

#include <sys/utsname.h>
#include "boxing.h"

namespace idris {

Value charCode(const Value& value) {
  switch (value->type) {
    case Closure::Type::Char:
      return box<Closure::Type::Int>(value->Char);
    case Closure::Type::String:
      return box<Closure::Type::Int>(value->String.front());
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
      return box<Closure::Type::Char>(static_cast<char32_t>(value->Int));
    case Closure::Type::Char:
      return value;
    case Closure::Type::BigInt:
      return box<Closure::Type::Char>(static_cast<char32_t>(value->BigInt));
    case Closure::Type::Word8:
      return box<Closure::Type::Char>(static_cast<char32_t>(value->Word8));
    case Closure::Type::Word16:
      return box<Closure::Type::Char>(static_cast<char32_t>(value->Word16));
    case Closure::Type::Word32:
      return box<Closure::Type::Char>(static_cast<char32_t>(value->Word32));
    case Closure::Type::Word64:
      return box<Closure::Type::Char>(static_cast<char32_t>(value->Word64));
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


} // namespace idris

