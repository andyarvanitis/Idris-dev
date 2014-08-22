#ifndef __idris_cpp_runtime_callbacks_h_
#define __idris_cpp_runtime_callbacks_h_

#include "box.h"
#include "types.h"
#include "vm.h"

namespace idris {

template <typename RetType, typename... ArgTypes>
RetType proxy_function(const weak_ptr<VirtualMachine>& vm_weak, 
                       const weak_ptr<BoxedValue>& con_weak, 
                       const IndexType& oldbase,
                       ArgTypes... args) {  

  void _idris__123_APPLY0_125_(IndexType,IndexType);

  // Make sure the vm hasn't been destroyed (nor the function con)
  auto vm = vm_weak.lock();
  auto con = con_weak.lock();
  //
  if (vm and con) {
    // Switch runtime to this vm. TODO: thread safety, etc..
    auto vm_previous = g_vm;
    g_vm = vm;
  
    // Create (empty) private stacks and make them the system's currently used ones.
    CallStack callstack;
    ArgStack argstack;
    g_vm->callstack.swap(callstack);
    g_vm->argstack.swap(argstack);
  
    auto res = con;
    const vector<BoxedValue> arglist = { args... };
  
    for (auto arg : arglist) {
      if (res->getTypeId() == 'C') {
        g_vm->valstack_top += 1;
        g_vm->valstack[g_vm->valstack_top] = res;
        g_vm->valstack[g_vm->valstack_top + 1] = make_shared<Closure>(arg);
        slide(2);
        g_vm->valstack_top = g_vm->valstack_base + 2;
        vmcall(_idris__123_APPLY0_125_,{oldbase,0});  
        while (g_vm->callstack.size()) {
          auto func = g_vm->callstack.top() ; g_vm->callstack.pop();
          auto fargs = g_vm->argstack.top() ; g_vm->argstack.pop();
          func(get<0>(fargs),get<1>(fargs));
        }  
        res = g_vm->ret;
      }
    }
  
    auto result = g_vm->ret;
  
    // Restore the stacks and vm
    g_vm->callstack.swap(callstack);
    g_vm->argstack.swap(argstack);    
    g_vm = vm_previous;

    return unbox<RetType>(result);

  } else {
    return RetType(0);
  }
}

} // namespace idris

#endif // __idris_cpp_runtime_callbacks_h_

