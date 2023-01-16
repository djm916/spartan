package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;

public final class Math
{
  public static Datum add(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.add(rhs);
    throw new TypeMismatch();
  }
    
  public static Datum sub(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.sub(rhs);
    throw new TypeMismatch();
  }
  
  public static Datum mul(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.mul(rhs);
    throw new TypeMismatch();
  }
  
  public static Datum div(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.div(rhs);
    throw new TypeMismatch();
  }
    
  public static Datum quotient(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.quotient(rhs);
    throw new TypeMismatch();
  }
  
  public static Datum remainder(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.remainder(rhs);
    throw new TypeMismatch();    
  }
  
  public static Datum neg(Datum x)
  {
    if (x instanceof INum arg)
      return arg.neg();
    throw new TypeMismatch();
  }
  
  public static Datum abs(Datum x)
  {
    if (x instanceof INum arg)
      return arg.abs();
    throw new TypeMismatch();
  }
  
  public static Datum floor(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.floor();
    throw new TypeMismatch();
  }
  
  public static Datum ceiling(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.ceiling();
    throw new TypeMismatch();
  }
  
  public static Datum round(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.round();
    throw new TypeMismatch();
  }
  
  public static Datum exp(Datum x, Datum y)
  {
    if (x instanceof INum base && y instanceof INum power)
      return base.exp(power);
    throw new TypeMismatch();
  }
  
  public static Datum log(Datum x, Datum y)
  {
    if (x instanceof INum arg && y instanceof INum base)
      return arg.log(base);
    throw new TypeMismatch();
  }
      
  public static INum sin(Datum x)
  {
    if (x instanceof INum arg)
      return arg.sin();
    throw new TypeMismatch();
  }
  
  public static INum cos(Datum x)
  {
    if (x instanceof INum arg)
      return arg.cos();
    throw new TypeMismatch();
  }
  
  public static Datum tan(Datum x)
  {
    if (x instanceof INum arg)
      return arg.tan();
    throw new TypeMismatch();
  }
  
  public static Datum asin(Datum x)
  {
    if (x instanceof INum arg)
      return arg.asin();
    throw new TypeMismatch();
  }
  
  public static Datum acos(Datum x)
  {
    if (x instanceof INum arg)
      return arg.acos();
    throw new TypeMismatch();
  }
  
  public static Datum atan(Datum x)
  {
    if (x instanceof INum arg)
      return arg.atan();
    throw new TypeMismatch();
  }
  
  public static final Complex makeComplex(Datum x, Datum y)
  {
    if (x instanceof IReal real && y instanceof IReal imag)
      return new Complex(real.doubleValue(), imag.doubleValue());
    throw new TypeMismatch();
  }
  
  public static final Ratio makeRatio(Datum x, Datum y)
  {
    if (x instanceof IInt numer && y instanceof IInt denom)
      return (Ratio) numer.div(denom);
    throw new TypeMismatch();
  }
}
