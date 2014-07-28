
public typealias i$Function = (Int, Int) -> ()

public struct i$FuncWrapper {
  let fun: i$Function  
  init(f: i$Function) {
    fun = f;
  }
}

public struct I$VM {  
  var valstack: [Any?] = []
  var valstack_top: Int = 0
  var valstack_base: Int = 0
  var ret: Any? = nil
  var callstack: [Any] = []
}  

public struct i$state {
  static var vm: I$VM = I$VM()
  static var valstack: [Any?] = []
  static var valstack_top: Int  = 0
  static var valstack_base: Int = 0
  static var ret: Any? = nil
  static var callstack: [Any] = []
}

public struct i$CON {
  var tag: Int
  var args: [Any?]
  var app: i$Function?
  var ev: i$Function?
  
  init(_ tag_: Int, _ args_: [Any?], _ app_: i$Function?, _ ev_: i$Function?) {
    tag = tag_
    args = args_
    app = app_
    ev = ev_
  }
}

public func i$SCHED(vm:I$VM) {
  i$state.vm = vm
  i$state.valstack = vm.valstack
  i$state.valstack_top = vm.valstack_top
  i$state.valstack_base = vm.valstack_base
  i$state.ret = vm.ret
  i$state.callstack = vm.callstack
}

public func i$SLIDE(argCount: Int) {
  for (var i = 0; i < argCount; ++i) {
    let stackPos = i$state.valstack_base + i
    if stackPos >= i$state.valstack.count {
      i$state.valstack.append(nil)
    }    
    i$state.valstack[stackPos] = i$state.valstack[i$state.valstack_top + i]
  }
}

public func i$PROJECT(val:Any?, loc:Int, arity:Int) {

  var args: [Any?] = []

  if let value = val { // if not nil
    let con = value as i$CON
    args = con.args
  }

  for (var i = 0; i < arity; ++i) {
    let stackPos = i$state.valstack_base + i + loc
    if stackPos >= i$state.valstack.count {
      i$state.valstack.append(nil)
    }    
    if args.count > 0 {
      i$state.valstack[stackPos] = args[i]
    }
  }
}

public func i$CALL(fun: i$Function, args: [Int]) {
  i$state.callstack.append(args);
  i$state.callstack.append(i$FuncWrapper(fun));
}
/*

var i$ffiWrap = function(fid,oldbase,myoldbase) {
  return function() {
    i$state.callstack = [];

    var res = fid;

    for(var i = 0; i < arguments.length; ++i) {
      while (res instanceof i$CON) {
        i$state.valstack_top += 1;
        i$state.valstack[i$state.valstack_top] = res;
        i$state.valstack[i$state.valstack_top + 1] = arguments[i];
        i$SLIDE(2);
        i$state.valstack_top = i$state.valstack_base + 2;
        i$CALL(_idris__123_APPLY0_125_,[oldbase])
        while (i$state.callstack.length) {
          var func = i$state.callstack.pop();
          var args = i$state.callstack.pop();
          func.apply(this,args);
        }
        res = i$state.ret;
      }
    }

    i$state.callstack = i$state.vm.callstack;

    return i$state.ret;
  }
}

var i$charCode = function(str) {
  if (typeof str == "string")
    return str.charCodeAt(0);
  else
    return str;
}

var i$fromCharCode = function(chr) {
  if (typeof chr == "string")
    return chr;
  else
    return String.fromCharCode(chr);
}
*/


public func i$putStr(s: Any?) {
  if let string = s {
    println(string);
  } else {
    println("nil");
  }
};


public func i$systemInfo(_:Int) -> String {
  return "Swift";
}


