package spartan.runtime;

import spartan.data.Symbol;
import spartan.data.Nil;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol name, Inst next)
  {
    super(next);
    this.name = name;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.globals.bind(name, vm.result);
    vm.result = Nil.Value;
    vm.control = next;
  }

  private final Symbol name;
}
