#ifndef __idris_cpp_runtime_exceptions_h_
#define __idris_cpp_runtime_exceptions_h_

#include <cassert>
#include <sstream>
#include <iostream>

#if __has_feature(cxx_exceptions)
#define RAISE(msg,value) {\
  struct idris_cpp_runtime : std::exception { \
    std::ostringstream message; \
    const char* what() const noexcept { \
      return message.str().c_str(); \
    } \
  }; \
  idris_cpp_runtime exception; \
  exception.message << msg << value; \
  throw exception; \
  }
#else
#define RAISE(msg,value) std::cout << "Idris C++ runtime assertion: " << msg << value << std::endl; assert(false);
#endif
  

#endif // __idris_cpp_runtime_exceptions_h_
