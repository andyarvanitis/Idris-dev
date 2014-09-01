#ifndef __idris_cpp_runtime_vm_h_
#define __idris_cpp_runtime_vm_h_

#include <deque>
#include <stack>
#include "types.h"

namespace idris {

//---------------------------------------------------------------------------------------
class ValueStack : public std::deque<Value> {
public:
  using std::deque<Value>::deque;  
  Value& operator [](size_type index) {
    if (index >= this->size()) {
      this->resize(index + 1, nullptr);
    }
    return this->std::deque<Value>::operator[](index);
  }
};

//---------------------------------------------------------------------------------------
using ArgsPair = pair<IndexType,IndexType>;

using CallStack = stack<Func>;
using ArgStack = stack<ArgsPair>;

//---------------------------------------------------------------------------------------
struct VirtualMachine {
  ValueStack valstack;
  IndexType valstack_top = 0;
  IndexType valstack_base = 0;
  Value ret = nullptr;
  CallStack callstack;
  ArgStack argstack;
};

//---------------------------------------------------------------------------------------

void slide(shared_ptr<VirtualMachine>& vm,
           const size_t num_args);

void project(shared_ptr<VirtualMachine>& vm,
             const Value& value, const IndexType loc, const int arity);

void vmcall(shared_ptr<VirtualMachine>& vm,
            const Func& fn, const ArgsPair& args);

//---------------------------------------------------------------------------------------

} // namespace idris

#endif // __idris_cpp_runtime_vm_h_

