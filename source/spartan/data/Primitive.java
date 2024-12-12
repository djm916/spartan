package spartan.data;

import spartan.runtime.VirtualMachine;

public non-sealed abstract class Primitive implements Datum, IFun
{
  public abstract void apply(VirtualMachine vm);
  
  @Override // IFun
  public Signature sig()
  {
    return sig;
  }
  
  protected Primitive(Signature sig)
  {
    this.sig = sig;
  }

  protected final Signature sig;
}
