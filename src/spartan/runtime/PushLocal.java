package spartan.runtime;

public class PushLocal extends Inst
{
  private final int numBindings;
  
  public PushLocal(int numBindings, Inst next)
  {
    super(next);
    this.numBindings = numBindings;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.locals = new LocalEnv(numBindings, vm.locals);
    vm.control = next;
  }
}
