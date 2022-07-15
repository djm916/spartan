package spartan.data;

public final class Bool extends Datum
{
  public static final Bool True = new Bool(true);  
  public static final Bool False = new Bool(false);
  
  public Type type()
  {
    return Type.Bool;
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
