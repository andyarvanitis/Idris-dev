#ifndef __idris_cpp_types_h_
#define __idris_cpp_types_h_

#include "box.h"

namespace idris {

using namespace std;  

using Int = BoxType<'i', int>;
extern template struct BoxType<'i', int>;

using BigInt = BoxType<'b', long long>;
extern template struct BoxType<'b', long long>;
  
using Float = BoxType<'f', double>;
extern template struct BoxType<'f', double>;

using String = BoxType<'s', string>;
extern template struct BoxType<'s', string>;

using Char = BoxType<'c', char32_t>;
extern template struct BoxType<'c', char32_t>;

using Word8 = BoxType<'1', uint8_t>;
extern template struct BoxType<'1', uint8_t>;

using Word16 = BoxType<'2', uint16_t>;
extern template struct BoxType<'2', uint16_t>;

using Word32 = BoxType<'4', uint32_t>;
extern template struct BoxType<'4', uint32_t>;

using Word64 = BoxType<'8', uint64_t>;
extern template struct BoxType<'8', uint64_t>;

using ManagedPtr = BoxType<'m', shared_ptr<void>>;
extern template struct BoxType<'m', shared_ptr<void>>;

using Ptr = BoxType<'p', void*>;
extern template struct BoxType<'p', void*>;

using Con = BoxType<'C', Constructor>;
extern template struct BoxType<'C', Constructor>;

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

