package spartan.data;

import spartan.errors.TypeMismatch;

public sealed interface IOrd
permits Int, BigInt, Ratio, Real, Text, Text.Cursor
{
  default int compareTo(IOrd rhs)
  {
    return switch (rhs) {
      case Int z -> compareTo(z);
      case BigInt z -> compareTo(z);
      case Ratio q -> compareTo(q);
      case Real r -> compareTo(r);
      case Text s -> compareTo(s);
      case Text.Cursor i -> compareTo(i);
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
  
  default int compareTo(Text rhs)
  {
    throw new TypeMismatch();
  }
  
  default int compareTo(Text.Cursor rhs)
  {
    throw new TypeMismatch();
  }
}
