package spartan.data;

public final class Nil implements Datum, IEq
{
  public static final Nil VALUE = new Nil();
  
  @Override // Datum
  public Type type()
  {
    return Type.NIL;
  }
  
  @Override // Datum
  public String repr()
  {
    return "#nil";
  }
  
  @Override // Datum
  public boolean boolValue()
  {
    return false;
  }
  
  @Override // IEq
  public boolean isEqual(Nil rhs)
  {
    return this == rhs;
  }
  
  private Nil() {}
}
