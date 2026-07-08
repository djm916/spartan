package spartan.runtime;

public final class Jump implements Inst
{
  public Jump()
  {
    this(null, null);
  }
  
  public Jump(Inst target, Inst next)
  {
    //this.target = target;
    setTarget(target);
    this.next = next;
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
  
  public Inst next()
  {
    return next;
  }
  
  public void setNext(Inst next)
  {
    this.next = next;
  }
  
  private Inst target;
  private Inst next;
}
