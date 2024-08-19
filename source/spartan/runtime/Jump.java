package spartan.runtime;

public final class Jump extends Inst
{
  public Jump()
  {
    super(null);
  }
  
  public Jump(Inst target)
  {
    super(null);
    this.target = target;
  }
  
  public final void setTarget(Inst target)
  {
    this.target = target;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.control = target;
  }
    
  Inst target;
}
