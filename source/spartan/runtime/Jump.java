package spartan.runtime;

public final class Jump extends Inst
{
  private Inst target;
  
  public Jump()
  {
    super(null);
  }
  
  public final void setTarget(Inst target)
  {
    this.target = target;
  }
  
  public final void exec(VirtualMachine vm)
  {
    vm.control = target;
  }
}
