package spartan.runtime;

public class PopLocal extends Inst
{
  public PopLocal(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.locals = vm.locals.parent;
    vm.control = next;
  }
}
