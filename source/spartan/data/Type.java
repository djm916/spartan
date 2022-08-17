package spartan.data;

public enum Type
{
  Nil("type/nil"),
  Bool("type/boolean"),
  Int("type/integer"),
  BigInt("type/integer"),
  Ratio("type/rational"),
  Real("type/real"),
  Complex("type/complex"),
  Symbol("type/symbol"),
  Text("type/text"),
  Bytes("type/bytes"),
  List("type/list"),
  Vector("type/vector"),
  Primitive("type/procedure"),
  Closure("type/procedure"),
  Macro("type/macro"),
  Continuation("type/continuation"),
  Port("type/port");
  
  public String getName()
  {
    return name;
  }
  
  private Type(String name)
  {
    this.name = name;
  }
  
  private final String name;
}
