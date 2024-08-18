package spartan.runtime;

import spartan.parsing.Position;

public final class PushFrame extends Inst
{
  public PushFrame(Inst returnTo, Position position, Inst next)
  {
    super(next);
    this.returnTo = returnTo;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.pushFrame(returnTo, position);
    vm.control = next;
  }

  public void emit(StringBuilder sb)
  {
    //sb.append("(push-frame)\n");
    //next.emit(sb);
  }
  
  public final Inst returnTo;
  public final Position position;
}
