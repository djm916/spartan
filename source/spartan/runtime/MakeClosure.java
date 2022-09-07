package spartan.runtime;

import spartan.data.Closure;
import spartan.compiling.ProcTemplate;

public final class MakeClosure extends Inst
{  
  public MakeClosure(ProcTemplate template, Inst next)
  {
    super(next);
    this.template = template;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = new Closure(template, vm.locals);
    vm.control = next;
  }

  private final ProcTemplate template;
}
