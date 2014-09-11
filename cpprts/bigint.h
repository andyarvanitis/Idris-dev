#ifndef __idris_bigint_h_
#define __idris_bigint_h_

#include <cstdint>

//---------------------------------------------------------------------------------------
// If using GCC or clang on a 64bit system, use their built-in 128bit integer
// as a quick-and-dirty big int.
//---------------------------------------------------------------------------------------
#if defined(__GNUC__) && UINTPTR_MAX == 0xffffffffffffffff
#define SIMPLE_BIGINT
  #include <ostream>
  namespace idris {
    using bigint_t = __int128_t;
    using ubigint_t = __uint128_t;
    std::ostream& operator << (std::ostream&, bigint_t);
    std::ostream& operator << (std::ostream&, bigint_t);
  } // namespace idris
#else
  #warning "Inadequate big integer support detected! (using long long)"
  using bigint_t = long long;
#endif

#endif // __idris_bigint_h_
