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
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {    
    vm.locals = locals;
    vm.control = body;
  }
  
  @Override // IFun
  public Signature signature()
  {
    return sig;
  }
    
  private final Inst body;
  private final Signature sig;
  private final LocalEnv locals;  
}
