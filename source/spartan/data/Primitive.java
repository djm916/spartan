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
  
  @Override // IFun
  public String type()
  {
    return "procedure";
  }
  
  @Override // IFun
  public boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  private final Signature sig;
}
