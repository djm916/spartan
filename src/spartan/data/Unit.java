package spartan.data;

public class Unit extends Value
{
  public static final Unit Instance = new Unit();
  
  public Type type()
  {
    return Type.Unit;
  }
  
  public String repr()
  {
    return "()";
  }
  
  private Unit() {}
}
