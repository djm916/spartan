package spartan.runtime;

public class PopArg extends Inst
{
  public PopArg(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.args.pop();
    vm.control = next;
  }
}
