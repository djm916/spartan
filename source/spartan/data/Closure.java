package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;
import spartan.runtime.VirtualMachine;
import spartan.compiling.ProcTemplate;

public final class Closure extends Callable
{
  private final Inst code;
  private final LocalEnv locals;
  
  public Closure(ProcTemplate template, LocalEnv locals)
  {
    super(template.requiredArgs, template.isVariadic);
    this.code = template.code;
    this.locals = locals;
  }
  
  public Type type()
  {
    return Type.Closure;
  }
  
  public void apply(VirtualMachine vm)
  {    
    vm.locals = locals;
    vm.control = code;
  }
}
