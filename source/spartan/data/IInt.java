package spartan.data;

/**
 * Extends the base numeric interface with a set of functions specific to integers.
 */
public sealed interface IInt extends INum
permits Int, BigInt
{
  // Conversion to native Java types
  int toInt32();
  long toInt64();
    
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
  
  @Override // Datum
  default String type()
  {
    return "integer";
  }
  
  String format(int base);
}
