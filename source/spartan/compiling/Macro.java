package spartan.compiling;

import spartan.data.Datum;
import spartan.data.List;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.errors.WrongNumberArgs;

class Macro
{
  Macro(Procedure proc)
  {
    this.body = proc.body();
    this.sig = proc.sig();
  }
  
  Datum apply(VirtualMachine vm, List args)
  {
    int numArgs = args.length();
    if (!arityMatches(numArgs))
      throw new WrongNumberArgs();
    vm.pushFrame(null, null);
    vm.args = args;
    vm.eval(body);
    return vm.result;
  }
  
  boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  private final Inst body;
  private final Signature sig;
}
