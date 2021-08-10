package spartan.data;

public class Nil extends Datum
{
  public static final Nil Instance = new Nil();
  
  public final Type type()
  {
    return Type.Nil;
  }
  
  public final String repr()
  {
    return "nil";
  }
}
