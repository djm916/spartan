package spartan.data;

import spartan.compiling.Signature;

public abstract class Primitive implements Datum, Callable
{
  public Primitive(int requiredArgs, boolean isVariadic)
  {
    this(new Signature(requiredArgs, isVariadic));
  }
  
  public Primitive(Signature sig)
  {
    this.sig = sig;
  }
  
  @Override
  public final Type type()
  {
    return Type.PRIMITIVE;
  }
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return sig.matches(numArgs);
  }
  
  private final Signature sig;
}
