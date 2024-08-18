package spartan.runtime;

import spartan.data.Datum;

public final class LoadConst extends Inst
{
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
  
  public void emit(StringBuilder sb)
  {
    sb.append("(load-const " + x.repr() + ")\n");
    //next.emit(sb);
  }

  private final Datum x;
}
