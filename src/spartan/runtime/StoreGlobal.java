package spartan.runtime;

import spartan.data.Symbol;

public final class StoreGlobal extends Inst
{
  private final Symbol symb;
  
  public StoreGlobal(Symbol symb, Inst next)
  {
    super(next);
    this.symb = symb;
  }
  
  public final void exec(VirtualMachine vm)
  {
    vm.globals.bind(symb, vm.result);
    vm.control = next;
  }
}
