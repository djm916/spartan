package spartan.data;

public sealed interface IEq
permits Nil, Bool, Symbol, Int, BigInt, Ratio, Float64, Complex, Text, Text.Cursor
{
  default boolean isEqual(IEq rhs)
  {
    return switch (rhs) {
      case Nil x -> isEqual(x);
      case Bool b -> isEqual(b);      
      case Symbol s -> isEqual(s);
      case Int z -> isEqual(z);
      case BigInt z -> isEqual(z);
      case Ratio q -> isEqual(q);
      case Float64 r -> isEqual(r);
      case Complex c -> isEqual(c);      
      case Text s -> isEqual(s);
      case Text.Cursor c -> isEqual(c);      
    };
  }
  
  default boolean isEqual(Int rhs)
  {
    return false;
  }
  
  default boolean isEqual(BigInt rhs)
  {
    return false;
  }
  
  default boolean isEqual(Ratio rhs)
  {
    return false;
  }
  
  default boolean isEqual(Float64 rhs)
  {
    return false;
  }
  
  default boolean isEqual(Complex rhs)
  {
    return false;
  }
  
  default boolean isEqual(Symbol rhs)
  {
    return false;
  }
    
  default boolean isEqual(Text rhs)
  {
    return false;
  }
  
  default boolean isEqual(Text.Cursor rhs)
  {
    return false;
  }
  
  default boolean isEqual(Bool rhs)
  {
    return false;
  }
  
  default boolean isEqual(Nil rhs)
  {
    return false;
  }  
}
