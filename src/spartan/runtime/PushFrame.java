package spartan.runtime;

import spartan.data.List;

public final class PushFrame extends Inst
{
  private final Inst returnTo;
  
  public PushFrame(Inst returnTo, Inst next)
  {
    super(next);
    this.returnTo = returnTo;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.pushFrame(returnTo);
    vm.control = next;
  }
}
