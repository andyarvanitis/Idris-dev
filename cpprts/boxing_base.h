#ifndef __idris_cpp_runtime_boxing_templates_h_
#define __idris_cpp_runtime_boxing_templates_h_

#include "types.h"
#include "exceptions.h"

namespace idris {

template <Closure::Type T>
struct unboxed {
};

template <>
struct unboxed<Closure::Type::Int> {
  using type = int;
};

template <>
struct unboxed<Closure::Type::BigInt> {
  using type = long long int;
};

template <>
struct unboxed<Closure::Type::Float> {
  using type = double;
};

template <>
struct unboxed<Closure::Type::String> {
  using type = string;
};

template <>
struct unboxed<Closure::Type::Char> {
  using type = char32_t;
};

template <>
struct unboxed<Closure::Type::Word8> {
  using type = uint8_t;
};

template <>
struct unboxed<Closure::Type::Word16> {
  using type = uint16_t;
};

template <>
struct unboxed<Closure::Type::Word32> {
  using type = uint32_t;
};

template <>
struct unboxed<Closure::Type::Word64> {
  using type = uint64_t;
};

template <>
struct unboxed<Closure::Type::ManagedPtr> {
  using type = shared_ptr<void>;
};

template <>
struct unboxed<Closure::Type::Ptr> {
  using type = void*;
};

//-------------------------------------------------------------------------------------------------

template <Closure::Type T>
void init_closure(Value&, typename unboxed<T>::type);

template <Closure::Type T>
Value box(typename unboxed<T>::type value) {
  auto closure = make_shared<Closure>(T);
  init_closure<T>(closure,value);
  return closure;
}

// C-style strings require some extra attention
//
template <Closure::Type T>
Value box(char* value);

template <Closure::Type T>
Value box(const char* value);

template <Closure::Type T>
typename unboxed<T>::type unbox(const Value& value);

} // namespace idris

#endif // __idris_cpp_runtime_boxing_templates_h_
