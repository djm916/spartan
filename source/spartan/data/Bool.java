package spartan.data;

public final class Bool implements Datum, IEq
{
  public static final Bool TRUE = new Bool(true);  
  public static final Bool FALSE = new Bool(false);
  
  public static Bool valueOf(boolean value)
  {
    return value ? TRUE : FALSE;
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.BOOLEAN;
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
  
  @Override
  public boolean boolValue()
  {
    return value;
  }
  
  private Bool(boolean value)
  {
    this.value = value;
  }
  
  private final boolean value;
}
