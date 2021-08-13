package spartan.runtime;

public final class PopArgs extends Inst
{
  public PopArgs(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.popArgs();
    vm.control = next;
  }
}
