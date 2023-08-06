package spartan.data;

import spartan.errors.TypeMismatch;

public sealed interface IOrd
permits Int, BigInt, Ratio, Real
{
  default int compareTo(IOrd rhs)
  {
    return switch (rhs) {
      case Int z -> compareTo(z);
      case BigInt z -> compareTo(z);
      case Ratio q -> compareTo(q);
      case Real r -> compareTo(r);
    };
  }
  
  default int compareTo(Int rhs)
  {
    throw new TypeMismatch();
  }
  
  default int compareTo(BigInt rhs)
  {
    throw new TypeMismatch();
  }
  
  default int compareTo(Ratio rhs)
  {
    throw new TypeMismatch();
  }
  
  default int compareTo(Real rhs)
  {
    throw new TypeMismatch();
  }
}
