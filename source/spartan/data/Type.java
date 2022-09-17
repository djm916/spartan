package spartan.data;

public enum Type
{
  Nil("type/nil"),
  Bool("type/boolean"),
  Int("type/integer"),
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
  
  public boolean isBool()
  {
    return this == Bool;
  }
  
  public boolean isInt()
  {
    return this == Int;
  }
  
  public boolean isReal()
  {
    return this == Real;
  }
  
  public boolean isComplex()
  {
    return this == Complex;
  }
  
  public boolean isNumber()
  {
    return this == Int || this == Real || this == Complex;
  }
  
  public boolean isSymbol()
  {
    return this == Symbol;
  }
  
  public boolean isText()
  {
    return this == Text;
  }
  
  public boolean isCallable()
  {
    return this == Primitive || this == Closure || this == Macro || this == Continuation;
  }
  
  private Type(String name)
  {
    this.name = name;
  }
  
  private final String name;
}
