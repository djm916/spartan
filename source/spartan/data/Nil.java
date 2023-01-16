package spartan.data;

public final class Nil implements Datum, IEq
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
  
  @Override
  public boolean isEqual(Nil rhs)
  {
    return this == rhs;
  }
  
  private Nil() {}
}
