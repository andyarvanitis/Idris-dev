#include "utility.h"

#include <sstream>
#include <iostream>
#include <codecvt>
#include <sys/utsname.h>

namespace idris {

using namespace std;

// Value charCode(const Value& value) {
//   return box<Int>(unbox<Char>(value));
// }
//
// Value fromCharCode(const Value& value) {
//   return box<Char>(unbox<Int>(value));
// }

char32_t utf8_head(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  return utf32.front();
 }

string utf8_tail(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  auto utf32_tail = utf32.substr(1, utf32.length() - 1);
  return utf32conv.to_bytes(utf32_tail);
}

char32_t char32_from_utf8_string(const string& str, size_t index) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  return utf32.at(index);
}

string utf8_reverse(const string& str) {
  static wstring_convert<codecvt_utf8<char32_t>, char32_t> utf32conv;
  auto utf32 = utf32conv.from_bytes(str);
  auto utf32_reversed = reverse(utf32);
  return utf32conv.to_bytes(utf32_reversed);  
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

