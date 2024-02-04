package spartan.compiling;

import spartan.runtime.VirtualMachine;
import spartan.runtime.Inst;
import spartan.data.Datum;
import spartan.data.List;
import spartan.errors.SourceInfo;
import spartan.errors.WrongNumberArgs;

public final class Macro
{
  public Macro(Procedure proc)
  {
    this.sig = proc.sig();
    this.body = proc.body();
  }
  
  public Datum expand(VirtualMachine vm, List args, SourceInfo source)
  {
    int numArgs = args.length();
    if (!sig.matches(numArgs))
      throw new WrongNumberArgs(source);
    vm.pushFrame(null, source.position());
    vm.args = args;
    vm.eval(body);
    return vm.result;
  }
  
  private final Signature sig;
  private final Inst body;
}
