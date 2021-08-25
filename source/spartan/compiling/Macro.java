package spartan.compiling;

import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.data.Datum;
import spartan.data.List;
import spartan.errors.RuntimeError;
import spartan.errors.WrongNumberArgs;

class Macro
{
  private final Inst code;
  private final int requiredArgs;
  private final boolean isVariadic;
  
  Macro(Inst code, int requiredArgs, boolean isVariadic)
  {
    this.code = code;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  Datum apply(VirtualMachine vm, List args) throws WrongNumberArgs, RuntimeError
  {
    int numArgs = args.length();
    
    if (numArgs < requiredArgs || !isVariadic && numArgs > requiredArgs)
      throw new WrongNumberArgs();
    
    vm.pushFrame(null);
    vm.args = args;
    return vm.eval(code);
  }
}
