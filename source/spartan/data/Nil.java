package spartan.data;

public final class Nil implements Datum
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
