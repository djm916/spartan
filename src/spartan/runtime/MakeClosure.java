package spartan.runtime;

import spartan.data.Closure;

public class MakeClosure extends Inst
{
  private final Inst code;
  private final int requiredArgs;
  private final boolean isVariadic;
  
  public MakeClosure(Inst code, int requiredArgs, boolean isVariadic, Inst next)
  {
    super(next);
    this.code = code;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = new Closure(code, vm.locals, requiredArgs, isVariadic);
    vm.control = next;
  }
}
