package spartan.runtime;

import spartan.data.Closure;
import spartan.compiling.Procedure;

public final class MakeClosure extends Inst
{  
  public MakeClosure(Procedure proc, Inst next)
  {
    super(next);
    this.proc = proc;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = new Closure(proc, vm.locals);
    vm.control = next;
  }
  
  final Procedure proc;
}
