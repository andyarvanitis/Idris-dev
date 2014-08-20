#ifndef __idris_cpp_runtime_boxing_h_
#define __idris_cpp_runtime_boxing_h_

#include "boxing_base.h"

namespace idris {

extern template 
void init_closure<Closure::Type::Int>(Value&, int);

extern template
void init_closure<Closure::Type::BigInt>(Value&, long long int);

extern template
Value box<Closure::Type::String>(string);

extern template
Value box<Closure::Type::String>(char*);

extern template
Value box<Closure::Type::String>(const char*);

extern template
void init_closure<Closure::Type::Char>(Value&, char32_t);

extern template
void init_closure<Closure::Type::Word8>(Value&, uint8_t);

extern template
void init_closure<Closure::Type::Word16>(Value&, uint16_t);

extern template
void init_closure<Closure::Type::Word32>(Value&, uint32_t);

extern template
void init_closure<Closure::Type::Word64>(Value&, uint64_t);

extern template
Value box<Closure::Type::ManagedPtr>(shared_ptr<void>);

extern template
void init_closure<Closure::Type::Ptr>(Value&, void*);

//---------------------------------------------------------------------------------------

extern template
int unbox<Closure::Type::Int>(const Value&);

extern template
double unbox<Closure::Type::Float>(const Value&);

extern template
string unbox<Closure::Type::String>(const Value&);

extern template
char32_t unbox<Closure::Type::Char>(const Value&);

extern template
long long int unbox<Closure::Type::BigInt>(const Value&);

extern template
uint8_t unbox<Closure::Type::Word8>(const Value&);

extern template
uint16_t unbox<Closure::Type::Word16>(const Value&);

extern template
uint32_t unbox<Closure::Type::Word32>(const Value&);

extern template
uint64_t unbox<Closure::Type::Word64>(const Value&);

extern template
shared_ptr<void> unbox<Closure::Type::ManagedPtr>(const Value&);

extern template
void* unbox<Closure::Type::Ptr>(const Value&);

} // namespace idris

#endif // __idris_cpp_runtime_boxing_h_

