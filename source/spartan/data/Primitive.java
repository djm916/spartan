package spartan.data;

public abstract class Primitive extends Callable
{
  protected Primitive(int requiredArgs, boolean isVariadic)
  {
    super(requiredArgs, isVariadic);
  }
  
  public final Type type()
  {
    return Type.PRIMITIVE;
  }
}
