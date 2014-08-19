#ifndef __idris_cpp_runtime_callbacks_h_
#define __idris_cpp_runtime_callbacks_h_

#include "types.h"
#include "vm.h"

namespace idris {

template <typename RetType, typename... ArgTypes>
RetType proxy_function(const weak_ptr<VirtualMachine>& vm_weak, 
                       const weak_ptr<Closure>& con_weak, 
                       const IndexType& oldbase,
                       ArgTypes... args);

} // namespace idris

#endif // __idris_cpp_runtime_callbacks_h_

