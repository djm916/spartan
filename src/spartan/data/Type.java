package spartan.data;

public enum Type
{
  Unit("unit"),
  Bool("bool"),
  Int("int"),
  Real("real"),
  List("list"),
  Tuple("tuple"),
  Record("record"),
  PrimFun("function"),
  Closure("function"),
  Text("text");
  
  public final String name;
  
  private Type(String name)
  {
    this.name = name;
  }
}
