package spartan.runtime;

import spartan.data.Symbol;

public final class LoadGlobal extends Inst
{
  private final Symbol symb;
  
  public LoadGlobal(Symbol symb, Inst next)
  {
    super(next);
    this.symb = symb;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.globals.lookup(symb);
    vm.control = next;
  }
}
