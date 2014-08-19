
#include "types.h"
#include "vm.h"
#include "main.h"

namespace idris {

int IdrisMain::argc = 0;
char **IdrisMain::argv = nullptr;

void _idris__123_runMain0_125_(IndexType,IndexType);

} // namespace idris

using namespace idris;

int main(int argc,char* argv[]) {
  IdrisMain::argc = argc;
  IdrisMain::argv = argv;
  auto vm = make_shared<VirtualMachine>();
  schedule(vm);
  _idris__123_runMain0_125_(0,0);
  while (g_vm->callstack.size() > 0) {
    auto func = g_vm->callstack.top() ; g_vm->callstack.pop();
    auto args = g_vm->argstack.top() ; g_vm->argstack.pop();
    func(get<0>(args),get<1>(args));
  };
}

