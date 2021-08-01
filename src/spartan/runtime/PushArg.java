package spartan.runtime;

public class PushArg extends Inst
{
  public PushArg(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.pushArg(vm.result);
    vm.control = next;
  }
}
