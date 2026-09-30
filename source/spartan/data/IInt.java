package spartan.data;

/**
 * Extends the base numeric interface with a set of functions specific to integers.
 */
public sealed interface IInt extends INum
permits Int, BigInt
{
  byte byteValue();
  int intValue();
  long longValue();
  
  default IInt quotient(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> quotient(z);
      case BigInt z -> quotient(z);
    };
  }
  
  IInt quotient(Int rhs);
  IInt quotient(BigInt rhs);
  
  default IInt remainder(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> remainder(z);
      case BigInt z -> remainder(z);
    };
  }
  
  IInt remainder(Int rhs);
  IInt remainder(BigInt rhs);
  
  default IRatio over(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> over(z);
      case BigInt z -> over(z);
    };
  }
  
  IRatio over(Int rhs);
  IRatio over(BigInt rhs);
  
  // Bitwise Operations
  
  IInt bitNot();
  
  default IInt bitAnd(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> bitAnd(z);
      case BigInt z -> bitAnd(z);
    };
  }
  
  IInt bitAnd(Int rhs);
  IInt bitAnd(BigInt rhs);
  
  default IInt bitOr(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> bitOr(z);
      case BigInt z -> bitOr(z);
    };
  }
  
  IInt bitOr(Int rhs);
  IInt bitOr(BigInt rhs);
  
  default IInt bitXor(IInt rhs)
  {
    return switch (rhs) {
      case Int z -> bitXor(z);
      case BigInt z -> bitXor(z);
    };
  }
  
  IInt bitXor(Int rhs);
  IInt bitXor(BigInt rhs);
  
  boolean isBitSet(int index);
  IInt setBit(int index);
  IInt clearBit(int index);
  IInt flipBit(int index);
  
  @Override // Datum
  default Type type()
  {
    return Type.INTEGER;
  }
  
  @Override // INum
  default boolean isInteger()
  {
    return true;
  }
  
  @Override // INum
  default boolean isReal()
  {
    return true;
  }
  
  @Override // INum
  default boolean isRational()
  {
    return true;
  }
  
  @Override // INum
  default boolean isComplex()
  {
    return true;
  }
  
  @Override // INum
  default boolean isFinite()
  {
    return true;
  }
  
  @Override // INum
  default boolean isNaN()
  {
    return false;
  }
  
  String formatInt(int base);
}
