package spartan.runtime;

public final class PopFrame extends Inst
{
  public PopFrame()
  {
    super(null);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.popFrame();
  }
}
