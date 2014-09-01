
#include <cassert>
#include "vm.h"

namespace idris {

//---------------------------------------------------------------------------------------

void slide(shared_ptr<VirtualMachine>& vm,
           const size_t num_args) {
  for (auto i=0; i < num_args; i++) {
    vm->valstack[vm->valstack_base + i] = vm->valstack[vm->valstack_top + i];
  }
}

void project(shared_ptr<VirtualMachine>& vm,
             const Value& value, const IndexType loc, const int arity) {
  assert(value and value->getTypeId() == 'C');
  const auto & args = unbox<Con>(value).args;
  for (auto i=0; i < arity; i++) {
    vm->valstack[vm->valstack_base + i + loc] = args[i];
  }
}

void vmcall(shared_ptr<VirtualMachine>& vm,
            const Func& fn, const ArgsPair& args) {
   vm->callstack.push(fn);
   vm->argstack.push(args);
}

} // namespace idris

