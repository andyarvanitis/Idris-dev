
#include <cassert>
#include "vm.h"

namespace idris {

//---------------------------------------------------------------------------------------
// The current (global) VM
// TODO: change to pointers and references, as appropriate; add thread safety, etc..
//
shared_ptr<VirtualMachine> g_vm = make_shared<VirtualMachine>();

//---------------------------------------------------------------------------------------

void schedule(shared_ptr<VirtualMachine>& vm) {
  g_vm = vm;
}

//---------------------------------------------------------------------------------------

void slide(const size_t num_args) {
  for (auto i=0; i < num_args; i++) {
    g_vm->valstack[g_vm->valstack_base + i] = g_vm->valstack[g_vm->valstack_top + i];
  }
}

void project(const Value& value, const IndexType loc, const int arity) {
  assert(value and value->getTypeId() == 'C');
  const auto & args = unbox<Con>(value).args;
  for (auto i=0; i < arity; i++) {
    g_vm->valstack[g_vm->valstack_base + i + loc] = args[i];
  }
}

void vmcall(const Func& fn, const ArgsPair& args) {
   g_vm->callstack.push(fn);
   g_vm->argstack.push(args);
}

} // namespace idris

