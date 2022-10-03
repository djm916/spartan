package spartan.runtime;

import spartan.data.Symbol;
import spartan.data.Nil;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol name, Inst next)
  {
    super(next);
    this.name = name;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.globals.lookupOrElse(name, Nil.VALUE);
    vm.control = next;
  }

  private final Symbol name;
}
