package spartan.data;

public enum Type
{
  Bool("bool"),
  Int("int"),
  Real("real"),
  List("list"),
  Vector("vector"),
  Record("record"),
  Symbol("symbol"),
  PrimFun("function"),
  Closure("function"),
  Text("text");
  
  public final String name;
  
  private Type(String name)
  {
    this.name = name;
  }
}
