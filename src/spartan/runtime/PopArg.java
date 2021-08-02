package spartan.runtime;

public final class PopArg extends Inst
{
  public PopArg(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.popArg();
    vm.control = next;
  }
}
