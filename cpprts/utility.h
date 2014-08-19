#ifndef __idris_cpp_runtime_utility_h_
#define __idris_cpp_runtime_utility_h_

#include <algorithm>
#include "types.h"

namespace idris {

Value charCode(const Value& value);

Value fromCharCode(const Value& value);

string systemInfo();

template <typename T>
inline T reverse(const T& container) {
  T rcontainer(container);
  std::reverse(rcontainer.begin(),rcontainer.end());
  return rcontainer;
}

} // namespace idris

#endif // __idris_cpp_runtime_utility_h_

