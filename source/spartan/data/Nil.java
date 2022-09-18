package spartan.data;

public class Nil extends Datum
{
  public static final Nil VALUE = new Nil();
  
  public Type type()
  {
    return Type.NIL;
  }
  
  public String repr()
  {
    return "nil";
  }
  
  private Nil() {}
}
