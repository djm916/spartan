package spartan.data;

import spartan.compiling.Signature;

public abstract class Primitive implements Datum, IFun
{
  public Primitive(int requiredArgs, boolean isVariadic)
  {
    this(new Signature(requiredArgs, isVariadic));
  }
  
  public Primitive(Signature sig)
  {
    this.sig = sig;
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public Signature signature()
  {
    return sig;
  }
    
  private final Signature sig;
}
