package spartan.data;

import java.math.BigInteger;

public sealed interface IInt extends INum
permits Int, BigInt
{
  int intValue();
  
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
  
}
