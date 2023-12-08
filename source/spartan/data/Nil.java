package spartan.data;

public final class Nil implements Datum, IEq
{
  public static final Nil VALUE = new Nil();
  
  @Override
  public Type type()
  {
    return TypeRegistry.NIL_TYPE;
  }
  
  @Override
  public String repr()
  {
    return "nil";
  }
  
  @Override
  public boolean isEqual(Nil rhs)
  {
    return this == rhs;
  }
  
  @Override
  public boolean boolValue()
  {
    return false;
  }
  
  private Nil() {}
}
