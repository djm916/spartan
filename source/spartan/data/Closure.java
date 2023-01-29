package spartan.data;

import spartan.compiling.Procedure;
import spartan.compiling.Signature;
import spartan.runtime.LocalEnv;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;

public final class Closure implements Datum, IFun
{
  public Closure(Procedure proc, LocalEnv locals)
  {
    this.body = proc.body();
    this.sig = proc.sig();
    this.locals = locals;
  }
  
  @Override
  public String type()
  {
    return "procedure";
  }
  
  @Override
  public void apply(VirtualMachine vm)
  {    
    vm.locals = locals;
    vm.control = body;
  }
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  private final Inst body;
  private final Signature sig;
  private final LocalEnv locals;  
}
