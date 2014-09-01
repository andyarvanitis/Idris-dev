
#include "types.h"
#include "vm.h"
#include "main.h"

namespace idris {

int IdrisMain::argc = 0;
char **IdrisMain::argv = nullptr;

void _idris__123_runMain0_125_(shared_ptr<VirtualMachine>&,IndexType,IndexType);

} // namespace idris

using namespace idris;

int main(int argc,char* argv[]) {
  IdrisMain::argc = argc;
  IdrisMain::argv = argv;
  auto vm = make_shared<VirtualMachine>();
  _idris__123_runMain0_125_(vm, 0, 0);
  while (vm->callstack.size() > 0) {
    auto func = vm->callstack.top();
    vm->callstack.pop();
    auto args = vm->argstack.top();
    vm->argstack.pop();
    func(vm, get<0>(args),get<1>(args));
  };
}
