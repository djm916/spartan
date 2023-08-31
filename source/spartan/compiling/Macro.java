package spartan.compiling;

import spartan.data.Datum;
import spartan.data.IFun;
import spartan.data.List;
import spartan.runtime.VirtualMachine;
import spartan.runtime.Inst;
import spartan.errors.WrongNumberArgs;

public final class Macro implements Datum, IFun
{
  Macro(Procedure proc)
  {
    this.sig = proc.sig();
    this.body = proc.body();
  }
  
  @Override // Datum
  public String type()
  {
    return "macro";
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {
    vm.eval(body);
  }
  
  @Override // IFun
  public boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  Datum expand(VirtualMachine vm, List args)
  {
    vm.pushFrame(null, null);
    vm.result = this;
    vm.args = args;
    vm.apply(args.length());
    return vm.result;
  }
  
  private final Signature sig;
  private final Inst body;
}
