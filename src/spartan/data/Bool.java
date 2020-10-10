package spartan.data;

public class Bool extends Value
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
  
  private final boolean value;
  
  private Bool(boolean value)
  {
    this.value = value;
  }
}
