package spartan.data;

public final class Bool implements Datum
{
  public static final Bool TRUE = new Bool(true);  
  public static final Bool FALSE = new Bool(false);
  
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
  
  private Bool(boolean value)
  {
    this.value = value;
  }
  
  private final boolean value;
}
