package spartan.data;

public final class Bool extends Datum
{
  public static final Bool TRUE = new Bool(true);  
  public static final Bool FALSE = new Bool(false);
  
  public Type type()
  {
    return Type.BOOL;
  }

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
