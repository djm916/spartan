package spartan.runtime;

public abstract class Inst
{
  protected final Inst next;
  
  protected Inst(Inst next)
  {
    this.next = next;
  }
  
  public abstract void eval(VirtualMachine vm);
}
