package spartan.data;

public final class Bool implements Datum, IEq
{
  public static final Bool TRUE = new Bool(true);  
  public static final Bool FALSE = new Bool(false);
  
  public static Bool of(boolean value)
  {
    return value ? TRUE : FALSE;
  }
  
  @Override
  public Type type()
  {
    return Type.BOOL;
  }

  @Override
  public String repr()
  {
    return value ? "true" : "false";
  }
  
  @Override
  public boolean isEqual(Bool rhs)
  {
    return this == rhs;
  }
  
  private Bool(boolean value)
  {
    this.value = value;
  }
  
  private final boolean value;
}
