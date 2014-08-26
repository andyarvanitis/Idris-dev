#ifndef __idris_cpp_types_h_
#define __idris_cpp_types_h_

#include "box.h"

namespace idris {

using namespace std;  

using Int = TypedBoxedValue<'i', int>;
extern template struct TypedBoxedValue<'i', int>;

using BigInt = TypedBoxedValue<'b', long long>;
extern template struct TypedBoxedValue<'b', long long>;
  
using Float = TypedBoxedValue<'f', double>;
extern template struct TypedBoxedValue<'f', double>;

using String = TypedBoxedValue<'s', string>;
extern template struct TypedBoxedValue<'s', string>;

using Char = TypedBoxedValue<'c', char32_t>;
extern template struct TypedBoxedValue<'c', char32_t>;

using Word8 = TypedBoxedValue<'1', uint8_t>;
extern template struct TypedBoxedValue<'1', uint8_t>;

using Word16 = TypedBoxedValue<'2', uint16_t>;
extern template struct TypedBoxedValue<'2', uint16_t>;

using Word32 = TypedBoxedValue<'4', uint32_t>;
extern template struct TypedBoxedValue<'4', uint32_t>;

using Word64 = TypedBoxedValue<'8', uint64_t>;
extern template struct TypedBoxedValue<'8', uint64_t>;

using ManagedPtr = TypedBoxedValue<'m', shared_ptr<void>>;
extern template struct TypedBoxedValue<'m', shared_ptr<void>>;

using Ptr = TypedBoxedValue<'p', void*>;
extern template struct TypedBoxedValue<'p', void*>;

using Con = TypedBoxedValue<'C', Constructor>;
extern template struct TypedBoxedValue<'C', Constructor>;

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

