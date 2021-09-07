package spartan.runtime;

public final class PopEnv extends Inst
{
  public PopEnv(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.locals = vm.locals.parent;
    vm.control = next;
  }
}
