package spartan.runtime;

public final class PushLocal extends Inst
{
  private final int numSlots;
  
  public PushLocal(int numSlots, Inst next)
  {
    super(next);
    this.numSlots = numSlots;
  }
  
  public final void exec(VirtualMachine vm)
  {
    vm.locals = new LocalEnv(numSlots, vm.locals);
    vm.control = next;
  }
}
