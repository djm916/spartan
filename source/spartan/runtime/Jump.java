package spartan.runtime;

public final class Jump implements Inst
{
  public Jump()
  {
    this(null);
  }
  
  public Jump(Inst target)
  {
    //this.target = target;
    setTarget(target);
  }
  
  public Inst target()
  {
    return target;
  }
  
  public void setTarget(Inst target)
  {
    //this.target = target;
    while ((target instanceof Jump j) && j.target != null)
      target = j.target;
    this.target = target;
  }
  
  private Inst target;
}
