package spartan.runtime;

import spartan.data.Promise;

public final class MakePromise extends Inst
{
  private final Inst code;
  
  public MakePromise(Inst code, Inst next)
  {
    super(next);
    this.code = code;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = new Promise(code, vm.locals);
    vm.control = next;
  }
}
