package spartan.runtime;

public class PopLocal extends Inst
{
  private final int n;
  
  public PopLocal(int n, Inst next)
  {
    super(next);
    this.n = n;
  }
  
  public void exec(VirtualMachine vm)
  {
    for (int i = 0; i < n; ++i)
      vm.locals = vm.locals.parent;
    vm.control = next;
  }
}
