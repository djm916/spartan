package spartan.runtime;

import spartan.data.Symbol;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol name, Inst next)
  {
    super(next);
    this.name = name;
  }
  
  public final void eval(VirtualMachine vm)
  {
    GlobalEnv.bind(name, vm.result);
    vm.control = next;
  }

  private final Symbol name;
}
