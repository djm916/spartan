package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;

public final class Math
{
  public static INum add(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.add(rhs);
    throw new TypeMismatch();
  }
    
  public static INum sub(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.sub(rhs);
    throw new TypeMismatch();
  }
  
  public static INum mul(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.mul(rhs);
    throw new TypeMismatch();
  }
  
  public static INum div(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.div(rhs);
    throw new TypeMismatch();
  }
    
  public static IInt quotient(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.quotient(rhs);
    throw new TypeMismatch();
  }
  
  public static IInt remainder(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.remainder(rhs);
    throw new TypeMismatch();    
  }
  
  public static INum neg(Datum x)
  {
    if (x instanceof INum arg)
      return arg.neg();
    throw new TypeMismatch();
  }
  
  public static INum abs(Datum x)
  {
    if (x instanceof INum arg)
      return arg.abs();
    throw new TypeMismatch();
  }
  
  public static IInt floor(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.floor();
    throw new TypeMismatch();
  }
  
  public static IInt ceiling(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.ceiling();
    throw new TypeMismatch();
  }
  
  public static IInt round(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.round();
    throw new TypeMismatch();
  }
  
  public static ITrans exp(Datum x, Datum y)
  {
    if (x instanceof ITrans base && y instanceof ITrans power)
      return base.exp(power);
    throw new TypeMismatch();
  }
  
  public static ITrans log(Datum x, Datum y)
  {
    if (x instanceof ITrans arg && y instanceof ITrans base)
      return arg.log(base);
    throw new TypeMismatch();
  }
  
  public static ITrans sin(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.sin();
    throw new TypeMismatch();
  }
  
  public static ITrans cos(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.cos();
    throw new TypeMismatch();
  }
  
  public static ITrans tan(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.tan();
    throw new TypeMismatch();
  }
  
  public static ITrans asin(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.asin();
    throw new TypeMismatch();
  }
  
  public static ITrans acos(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.acos();
    throw new TypeMismatch();
  }
  
  public static ITrans atan(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.atan();
    throw new TypeMismatch();
  }
  
  public static final IComplex makeComplex(Datum x, Datum y)
  {
    if (x instanceof IReal real && y instanceof IReal imag)
      return new Complex(real, imag);
    throw new TypeMismatch();
  }
  
  public static final IRatio makeRatio(Datum x, Datum y)
  {
    if (x instanceof IInt numer && y instanceof IInt denom)
      return numer.over(denom);
    throw new TypeMismatch();
  }
}
