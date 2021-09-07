package spartan.runtime;

public final class PushEnv extends Inst
{
  private final int numSlots;
  
  public PushEnv(int numSlots, Inst next)
  {
    super(next);
    this.numSlots = numSlots;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.locals = new LocalEnv(numSlots, vm.locals);
    vm.control = next;
  }
}
