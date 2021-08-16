package spartan.data;

public enum Type
{
  Nil("type/nil"),
  Bool("type/bool"),
  Int("type/int"),
  Real("type/real"),
  Symbol("type/symbol"),
  Text("type/text"),
  List("type/list"),
  Vector("type/vector"),
  Record("type/record"),
  PrimFun("type/function"),
  Closure("type/function"),
  Promise("type/promise"),
  Port("type/port");
  
  public final String name;
  
  private Type(String name)
  {
    this.name = name;
  }
}
