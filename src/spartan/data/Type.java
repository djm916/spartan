package spartan.data;

public enum Type
{
  Nil("nil"),
  Bool("bool"),
  Int("int"),
  Real("real"),
  Symbol("symbol"),
  Text("text"),
  List("list"),
  Vector("vector"),
  Record("record"),
  PrimFun("function"),
  Closure("function");
  
  public final String name;
  
  private Type(String name)
  {
    this.name = name;
  }
}
