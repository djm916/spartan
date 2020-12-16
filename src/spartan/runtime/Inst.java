package spartan.runtime;

import spartan.errors.RuntimeError;

public abstract class Inst
{
  protected final Inst next;
  
  public Inst(Inst next)
  {
    this.next = next;
  }
  
  public abstract void exec(VirtualMachine vm) throws RuntimeError;
}
