package spartan.runtime;

public class PopFrame extends Inst
{
  public PopFrame()
  {
    super(null);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.control = vm.frame.returnTo;
    vm.locals = vm.frame.locals;
    vm.args = vm.frame.args;
    vm.frame = vm.frame.parent;
  }
}
