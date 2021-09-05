package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

public final class Closure extends Callable
{
  private final Inst code;
  private final LocalEnv locals;
  
  public Closure(Inst code, LocalEnv locals, int requiredArgs, boolean isVariadic)
  {
    super(requiredArgs, isVariadic);
    this.code = code;
    this.locals = locals;
  }
  
  public Type type()
  {
    return Type.Closure;
  }
  
  public void apply(VirtualMachine vm) throws Error
  {    
    vm.locals = locals;
    vm.control = code;
  }
}
