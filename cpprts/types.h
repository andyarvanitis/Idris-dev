#ifndef __idris_cpp_types_h_
#define __idris_cpp_types_h_

#include "box.h"
#include "types_aliases.h"
#include "types_extern.h"

namespace idris {

using namespace std;  

//---------------------------------------------------------------------------------------
// Special handling for C-style string conversions
//---------------------------------------------------------------------------------------

template <>
inline auto box<String, const char*>(const char * && s) -> Value {
  return s ? static_pointer_cast<BoxedValue>(make_shared<String>(string(s))) : nullptr;
}

template <>
inline auto box<String, char*>(char * && s) -> Value {
  return s ? static_pointer_cast<BoxedValue>(make_shared<String>(string(s))) : nullptr;
}

} // namespace idris


#endif // __idris_cpp_types_h_

