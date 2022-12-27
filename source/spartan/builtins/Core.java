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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).isEqual((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().isEqual((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().isEqual((Ratio)y);
          case REAL:    return ((Int)x).toReal().isEqual((Real)y);
          case COMPLEX: return ((Int)x).toComplex().isEqual((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).isEqual(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).isEqual((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().isEqual((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().isEqual((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().isEqual((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).isEqual(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).isEqual(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).isEqual((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().isEqual((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().isEqual((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).isEqual(((Int)y).toReal());
          case BIGINT:  return ((Real)x).isEqual(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).isEqual(((Ratio)y).toReal());
          case REAL:    return ((Real)x).isEqual((Real)y);
          case COMPLEX: return ((Real)x).toComplex().isEqual((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).isEqual(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).isEqual(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).isEqual(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).isEqual(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).isEqual((Complex)y);
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).isEqual((Text)y);
        }
        break;
      }
      case VECTOR: {
        switch (y.type()) {
          case VECTOR: return ((Vector)x).isEqual((Vector)y);
        }
        break;
      }
      case LIST: {
        switch (y.type()) {
          case LIST: return ((List)x).isEqual((List)y);
        }
        break;
      }
      case SYMBOL:
      case BOOL:
      case NIL: return x == y;
    }
    return false;
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
