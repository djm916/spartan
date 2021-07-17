package spartan.data;

public class Nil extends Value
{
  public static final Nil Instance = new Nil();
  
  public Type type()
  {
    return Type.Nil;
  }
  
  public String repr()
  {
    return "nil";
  }
}
