package spartan.data;

public final class Nil extends Datum
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
