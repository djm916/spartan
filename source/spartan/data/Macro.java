package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.common.Procedure;
import spartan.common.Signature;

public final class Macro implements Datum, Callable
{ 
  public Macro(Procedure proc)
  {
    this.body = proc.body();
    this.sig = proc.sig();
  }
  
  @Override
  public Type type()
  {
    return Type.MACRO;
  }
  
  @Override
  public void apply(VirtualMachine vm)
  {
    vm.control = body;
  }
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  private final Inst body;
  private final Signature sig;
}
