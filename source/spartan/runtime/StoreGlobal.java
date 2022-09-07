package spartan.runtime;

import spartan.data.Symbol;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol symb, Inst next)
  {
    super(next);
    this.symb = symb;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.globals.bind(symb, vm.result);
    vm.control = next;
  }

  private final Symbol symb;
}
