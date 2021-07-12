package spartan.runtime;

import spartan.data.Closure;

public class MakeClosure extends Inst
{
  private final Inst code;
  private final int numArgs;
  
  public MakeClosure(Inst code, int numArgs, Inst next)
  {
    super(next);
    this.numArgs = numArgs;
    this.code = code;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = new Closure(code, numArgs, vm.locals);
    vm.control = next;
  }
}
