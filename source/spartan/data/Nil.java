package spartan.data;

public final class Nil extends Datum
{
  public static final Nil Value = new Nil();
  
  public Type type()
  {
    return Type.Nil;
  }
  
  public String repr()
  {
    return "nil";
  }
}
