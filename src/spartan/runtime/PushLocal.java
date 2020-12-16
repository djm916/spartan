package spartan.runtime;

public class PushLocal extends Inst
{
  private final int n;
  
  public PushLocal(int n, Inst next)
  {
    super(next);
    this.n = n;
  }
  
  public void exec(VirtualMachine vm)
  {
    for (int i = 0; i < n; ++i)
      vm.locals = new LocalEnv(null, vm.locals);
    vm.control = next;
  }
}
