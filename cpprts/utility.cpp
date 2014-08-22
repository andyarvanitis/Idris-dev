#include "utility.h"

#include <sstream>
#include <iostream>
#include <sys/utsname.h>

namespace idris {

// Value charCode(const Value& value) {
//   return box<Int>(unbox<Char>(value));
// }
//
// Value fromCharCode(const Value& value) {
//   return box<Char>(unbox<Int>(value));
// }

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

