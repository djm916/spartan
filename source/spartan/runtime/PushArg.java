package spartan.runtime;

public final class PushArg extends Inst
{
  public PushArg(Inst next)
  {
    super(next);
  }
  
  public final void exec(VirtualMachine vm)
  {
    vm.pushArg(vm.result);
    vm.control = next;
  }
}
