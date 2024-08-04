package spartan.data;

public final class Void implements Datum, IEq
{
  public static final Void VALUE = new Void();
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.VOID_TYPE;
  }
  
  @Override // Datum
  public String repr()
  {
    return "void";
  }
  
  @Override // Datum
  public boolean boolValue()
  {
    return false;
  }
  
  @Override // IEq
  public boolean isEqual(Void rhs)
  {
    return this == rhs;
  }
  
  private Void() {}
}
