package spartan.runtime;

public class PushArg extends Inst
{
  public PushArg(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.args.push(vm.result);
    vm.control = next;
  }
}
