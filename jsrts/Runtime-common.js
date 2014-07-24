class I_VM 
  attr_accessor :valstack, :valstack_top, :valstack_base, :ret, :callstack
  def initialize()
    @valstack = []
    @valstack_top = 0
    @valstack_base = 0
    @ret = nil
    @callstack = []
  end
end

i_valstack = []
i_callstack = []

class I_CON
  attr_accessor :tag, :args, :app, :ev
  def initialize(tag,args,app,ev)
    @tag = tag
    @args = args
    @app = app
    @ev = ev
  end
end

def i_SCHED(vm)
  $i_vm = vm
  $i_valstack = vm.valstack
  $i_valstack_top = vm.valstack_top
  $i_valstack_base = vm.valstack_base
  $i_ret = vm.ret
  $i_callstack = vm.callstack
end

def i_SLIDE(args)
  for i in 0 ... args
    $i_valstack[$i_valstack_base + i] = $i_valstack[$i_valstack_top + i]
  end
end

def i_PROJECT(val,loc,arity)
  for i in 0 ... arity
    $i_valstack[$i_valstack_base + i + loc] = val.args[i]
  end
end

def i_CALL(fun,args)
   $i_callstack.push(args)
   $i_callstack.push(fun)
 end

# var i$ffiWrap = function(fid,oldbase,myoldbase) {
#   return function() {
#     i$callstack = [];
#
#     var res = fid;
#
#     for(var i = 0; i < arguments.length; ++i) {
#       while (res instanceof i$CON) {
#         i$valstack_top += 1;
#         i$valstack[i$valstack_top] = res;
#         i$valstack[i$valstack_top + 1] = arguments[i];
#         i$SLIDE(2);
#         i$valstack_top = i$valstack_base + 2;
#         i$CALL(_idris__123_APPLY0_125_,[oldbase])
#         while (i$callstack.length) {
#           var func = i$callstack.pop();
#           var args = i$callstack.pop();
#           func.apply(this,args);
#         }
#         res = i$ret;
#       }
#     }
#
#     i$callstack = i$vm.callstack;
#
#     return i$ret;
#   }
# }
#
# var i$charCode = function(str) {
#   if (typeof str == "string")
#     return str.charCodeAt(0);
#   else
#     return str;
# }
#
# var i$fromCharCode = function(chr) {
#   if (typeof chr == "string")
#     return chr;
#   else
#     return String.fromCharCode(chr);
# }


