package spartan.runtime;

public final class Nop implements Inst
{
  public Nop() {}
  
  public Inst next()
  {
    return next;
  }
  
  public void setNext(Inst next)
  {
    this.next = next;
  }
  
  private Inst next;
}
