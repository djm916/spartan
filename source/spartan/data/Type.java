package spartan.data;

public enum Type
{
  Nil("type/nil"),
  Bool("type/bool"),
  Int("type/int"),
  Ratio("type/ratio"),
  Real("type/real"),
  Complex("type/complex"),
  Symbol("type/symbol"),
  Text("type/text"),
  List("type/list"),
  Vector("type/vector"),
  Primitive("type/procedure"),
  Closure("type/procedure"),
  Macro("type/macro"),
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
