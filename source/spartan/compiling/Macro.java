package spartan.compiling;

import spartan.data.Datum;
import spartan.data.List;
import spartan.runtime.VirtualMachine;
import spartan.runtime.Inst;
import spartan.errors.WrongNumberArgs;

public final class Macro
{
  Macro(Procedure proc)
  {
    this.sig = proc.sig();
    this.body = proc.body();
  }
  
  Datum apply(VirtualMachine vm, List args)
  {
    int numArgs = args.length();
    if (!sig.matches(numArgs))
      throw new WrongNumberArgs();
    vm.pushFrame(null, null);
    vm.args = args;
    vm.eval(body);
    return vm.result;
  }
  
  private Signature sig;
  private Inst body;
}
