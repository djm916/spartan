package spartan.data;

public enum Type
{
  NIL("type/nil"),
  BOOL("type/boolean"),
  INT("type/integer"),
  BIGINT("type/integer"),
  RATIO("type/rational"),
  REAL("type/real"),
  COMPLEX("type/complex"),
  SYMBOL("type/symbol"),
  TEXT("type/text"),
  BYTES("type/bytes"),
  LIST("type/list"),
  VECTOR("type/vector"),
  PRIMITIVE("type/procedure"),
  CLOSURE("type/procedure"),
  MACRO("type/macro"),
  CONTINUE("type/continuation"),
  PORT("type/port");
  
  public Symbol toSymbol()
  {
    return Symbol.of(name);
  }
  
  @Override
  public String toString()
  {
    return name;
  }
  
  public boolean isBool()
  {
    return this == BOOL;
  }
  
  public boolean isInt()
  {
    return this == INT || this == BIGINT;
  }
  
  public boolean isRatio()
  {
    return this == RATIO;
  }
  
  public boolean isReal()
  {
    return this == REAL;
  }
  
  public boolean isComplex()
  {
    return this == COMPLEX;
  }
  
  public boolean isNumber()
  {
    return this == INT || this == BIGINT || this == RATIO || this == REAL || this == COMPLEX;
  }
  
  public boolean isSymbol()
  {
    return this == SYMBOL;
  }
  
  public boolean isText()
  {
    return this == TEXT;
  }
  
  public boolean isCallable()
  {
    return this == PRIMITIVE || this == CLOSURE || this == MACRO || this == CONTINUE;
  }
  
  private Type(String name)
  {
    this.name = name;
  }
  
  private final String name;
}
