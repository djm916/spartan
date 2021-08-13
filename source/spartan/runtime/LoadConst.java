package spartan.runtime;

import spartan.data.Datum;

public final class LoadConst extends Inst
{
  private final Datum x;
  
  public LoadConst(Datum x, Inst next)
  {
    super(next);
    this.x = x;
  }
  
  public void eval(VirtualMachine vm)
  {
    vm.result = x;
    vm.control = next;
  }
}
