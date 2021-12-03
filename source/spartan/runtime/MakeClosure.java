package spartan.runtime;

import spartan.data.Closure;
import spartan.compiling.ProcTemplate;

public final class MakeClosure extends Inst
{
  private final ProcTemplate template;
  
  public MakeClosure(ProcTemplate template, Inst next)
  {
    super(next);
    this.template = template;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = new Closure(template.code,
                            vm.locals,
                            template.requiredArgs,
                            template.isVariadic);
    vm.control = next;
  }
}
