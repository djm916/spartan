package spartan.compiling;

import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
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
  
  void apply(VirtualMachine vm, int numArgs) throws WrongNumberArgs
  {
    if (numArgs < requiredArgs || !isVariadic && numArgs > requiredArgs)
      throw new WrongNumberArgs();
    vm.control = code;
  }
}
