package spartan.runtime;

import spartan.data.Closure;

public class MakeClosure extends Inst
{
  private final Inst code;
  
  public MakeClosure(Inst code, Inst next)
  {
    super(next);
    this.code = code;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = new Closure(code, vm.locals);
    vm.control = next;
  }
}
