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
  MAPPING("type/mapping"),
  PRIMITIVE("type/procedure"),
  CLOSURE("type/procedure"),
  MACRO("type/macro"),
  CONTINUE("type/continuation"),
  PORT("type/port"),
  WRAPPED(null);
  
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
    return isInt() || this == RATIO;
  }
  
  public boolean isReal()
  {
    return isRatio() || this == REAL;
  }
  
  public boolean isComplex()
  {
    return isReal() || this == COMPLEX;
  }
  
  public boolean isNumber()
  {
    return isComplex();
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
    return this == PRIMITIVE || this == CLOSURE || this == MACRO || this == CONTINUE || this == VECTOR || this == MAPPING;
  }
    
  public boolean isList()
  {
    return this == LIST;
  }
  
  public boolean isVector()
  {
    return this == VECTOR;
  }
  
  private Type(String name)
  {
    this.name = name;
  }
  
  private final String name;
}
