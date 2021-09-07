package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;
import spartan.runtime.VirtualMachine;

public final class Promise extends Callable
{
  private final Inst code;
  private final LocalEnv locals;
  
  public Promise(Inst code, LocalEnv locals)
  {
    super(0, false);
    this.code = code;
    this.locals = locals;
  }
  
  public Type type()
  {
    return Type.Promise;
  }
  
  public void apply(VirtualMachine vm)
  {    
    vm.locals = locals;
    vm.control = code;
  }
}
