package spartan.runtime;

public final class PushFrame extends Inst
{
  private final Inst returnTo;
  // TODO: Add position of call here to enable stack trace
  
  public PushFrame(Inst returnTo, Inst next)
  {
    super(next);
    this.returnTo = returnTo;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.pushFrame(returnTo);
    vm.control = next;
  }
}
