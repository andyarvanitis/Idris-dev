#ifndef __idris_cpp_runtime_boxing_h_
#define __idris_cpp_runtime_boxing_h_

#include "types.h"

namespace idris {

template <typename T>
Value box(T value) {
  return Closure::Box<T>(value);
}

inline Value MakeCon(const size_t tag, const vector<Value>& args, const Func& function){
  return make_shared<Closure>(make_shared<Constructor>(tag,args,function));
}

template <typename T>
T unbox(const Value& value) {
  RAISE("unknown underlying type when unboxing value of type ", int(value->type));
  return T();
}

} // namespace idris

#endif // __idris_cpp_runtime_boxing_h_

