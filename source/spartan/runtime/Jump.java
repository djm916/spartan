package spartan.runtime;

public final class Jump implements Inst
{
  public Jump()
  {
    this(null);
  }
  
  public Jump(Inst target)
  {
    this.target = target;
  }
  
  public Inst target()
  {
    return target;
  }
  
  public void setTarget(Inst target)
  {
    this.target = target;
  }
  
  private Inst target;
}
