package spartan.runtime;

public final class Jump extends Inst
{
  public Jump()
  {
    super(null);
  }
  
  public final void setTarget(Inst target)
  {
    this.target = target;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.control = target;
  }
  
  private Inst target;
}
