package spartan.runtime;

public final class PopLocal extends Inst
{
  public PopLocal(Inst next)
  {
    super(next);
  }
  
  public final void exec(VirtualMachine vm)
  {
    vm.locals = vm.locals.parent;
    vm.control = next;
  }
}
