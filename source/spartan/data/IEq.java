package spartan.data;

public sealed interface IEq
permits Nil, Bool, Symbol, Text, Int, BigInt, Ratio, Real, Complex
{
  default boolean isEqual(IEq rhs)
  {
    return switch (rhs) {
      case Int z -> isEqual(z);
      case BigInt z -> isEqual(z);
      case Ratio q -> isEqual(q);
      case Real r -> isEqual(r);
      case Complex c -> isEqual(c);
      case Symbol s -> isEqual(s);
      case Text s -> isEqual(s);
      case Bool b -> isEqual(b);
      case Nil x -> isEqual(x);      
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
  
  default boolean isEqual(Real rhs)
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
  
  default boolean isEqual(Bool rhs)
  {
    return false;
  }
  
  default boolean isEqual(Nil rhs)
  {
    return false;
  }  
}
