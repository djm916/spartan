package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;

public final class Core
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.TRUE : Bool.FALSE;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.FALSE || x == Nil.VALUE);
  }
  
  public static Bool not(Datum x)
  {
    return truth(!truth(x));
  }
  
  public static boolean isEqual(Datum x, Datum y)
  {
    if (x instanceof IEq lhs && y instanceof IEq rhs)
      return lhs.isEqual(rhs);
    throw new TypeMismatch();
  }
  
  public static int compareTo(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).compareTo((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().compareTo((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().compareTo((Ratio)y);
          case REAL:    return ((Int)x).toReal().compareTo((Real)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).compareTo(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).compareTo((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().compareTo((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().compareTo((Real)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).compareTo(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).compareTo(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).compareTo((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().compareTo((Real)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).compareTo(((Int)y).toReal());
          case BIGINT:  return ((Real)x).compareTo(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).compareTo(((Ratio)y).toReal());
          case REAL:    return ((Real)x).compareTo((Real)y);
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).compareTo((Text)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static int length(Datum x)
  {
    return switch (x.type()) {
      case LIST   -> ((List)x).length();
      case VECTOR -> ((Vector)x).length();
      case TEXT   -> ((Text)x).length();
      //case BYTES  -> ((Bytes)x).length();
      default     -> throw new TypeMismatch();
    };
  }
  
  public static Datum max(List xs)
  {
    var max = xs.car();
    xs = xs.cdr();
    while (xs != List.EMPTY) {
      var x = xs.car();
      if (compareTo(x, max) > 0)
        max = x;
      xs = xs.cdr();
    }
    return max;
  }
    
  public static Datum min(List xs)
  {
    var min = xs.car();
    xs = xs.cdr();
    while (xs != List.EMPTY) {
      var x = xs.car();
      if (compareTo(x, min) < 0)
        min = x;
      xs = xs.cdr();
    }
    return min;
  }
}
