package spartan.runtime;

public final class PopFrame extends Inst
{
  public PopFrame()
  {
    super(null);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.popFrame();
  }
}
