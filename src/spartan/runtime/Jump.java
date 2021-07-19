package spartan.runtime;

public class Jump extends Inst
{
  private Inst target;
  
  public Jump()
  {
    super(null);
  }
  
  public void setTarget(Inst target)
  {
    this.target = target;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.control = target;
  }
}
