package spartan.data;

import spartan.errors.IntegerOverflow;

public final class Real implements Datum, INum, IReal, IComplex, ITrans, IEq, IOrd
{
  public static final Real PI = new Real(Math.PI);
  public static final Real E = new Real(Math.E);
  public static final Real POS_INF = new Real(Double.POSITIVE_INFINITY);
  public static final Real NEG_INF = new Real(Double.NEGATIVE_INFINITY);
  public static final Real NAN = new Real(Double.NaN);
  public static final Real ZERO = new Real(0.0);
  
  public Real(double value)
  {
    this.value = value;
  }
  
  public Real(String val)
  {
    this(Double.parseDouble(val));
  }
  
  @Override
  public Type type()
  {
    return Type.REAL;
  }
  
  public String repr()
  {
    if (value == Double.POSITIVE_INFINITY)
      return "+inf";
    else if (value == Double.NEGATIVE_INFINITY)
      return "-inf";
    else if (value == Double.NaN)
      return "NaN";
    else
      return Double.toString(value);
  }
    
  private static double round(double x, int n)
  {
    double scale = Math.pow(10.0, n);
    double result = Math.floor(x * scale + 0.5) / scale;
    return result;
  }
  
  @Override
  public String formatDec(int precision)
  {
    return Double.toString(round(this.value, precision));
  }
  
  public Complex toComplex()
  {
    return new Complex(value, 0.0);
  }
  
  @Override
  public double doubleValue()
  {
    return value;
  }
  
  @Override
  public Real neg()
  {
    return new Real(-value);
  }
  
  @Override
  public Real abs()
  {
    return new Real(Math.abs(value));
  }
  
  @Override
  public Int floor()
  {
    return Int.valueOf((long) Math.floor(value));
  }
  
  @Override
  public Int ceiling()
  {
    return Int.valueOf((long) Math.ceil(value));
  }
  
  @Override
  public Int round()
  {
    return Int.valueOf((long) Math.round(value));
  }
  
  public Real sin()
  {
    return new Real(Math.sin(value));
  }
  
  public Real cos()
  {
    return new Real(Math.cos(value));
  }
  
  public Real tan()
  {
    return new Real(Math.tan(value));
  }
  
  public Real asin()
  {
    return new Real(Math.asin(value));
  }
  
  public Real acos()
  {
    return new Real(Math.acos(value));
  }
  
  public Real atan()
  {
    return new Real(Math.atan(value));
  }
  
  @Override
  public Real add(Int rhs)
  {
    return add(rhs.toReal());
  }
  
  @Override
  public Real add(BigInt rhs)
  {
    return add(rhs.toReal());
  }
  
  @Override
  public Real add(Ratio rhs)
  {
    return add(rhs.toReal());
  }
  
  @Override
  public Real add(Real that)
  {
    return new Real(this.value + that.value);
  }
  
  @Override
  public Complex add(Complex rhs)
  {
    return toComplex().add(rhs);
  }
  
  @Override
  public Real sub(Int rhs)
  {
    return sub(rhs.toReal());
  }
  
  @Override
  public Real sub(BigInt rhs)
  {
    return sub(rhs.toReal());
  }
  
  @Override
  public Real sub(Ratio rhs)
  {
    return sub(rhs.toReal());
  }
  
  @Override
  public Real sub(Real that)
  {
    return new Real(this.value - that.value);
  }
  
  @Override
  public Complex sub(Complex rhs)
  {
    return toComplex().sub(rhs);
  }
  
  @Override
  public Real mul(Int rhs)
  {
    return mul(rhs.toReal());
  }
  
  @Override
  public Real mul(BigInt rhs)
  {
    return mul(rhs.toReal());
  }
  
  @Override
  public Real mul(Ratio rhs)
  {
    return mul(rhs.toReal());
  }
  
  public Real mul(Real that)
  {
    return new Real(this.value * that.value);
  }
  
  @Override
  public Complex mul(Complex rhs)
  {
    return toComplex().mul(rhs);
  }
  
  @Override
  public Real div(Int rhs)
  {
    return div(rhs.toReal());
  }
  
  @Override
  public Real div(BigInt rhs)
  {
    return div(rhs.toReal());
  }
  
  @Override
  public Real div(Ratio rhs)
  {
    return div(rhs.toReal());
  }
  
  @Override
  public Real div(Real rhs)
  {
    return new Real(this.value / rhs.value);
  }
  
  @Override
  public Complex div(Complex rhs)
  {
    return toComplex().div(rhs);
  }
    
  @Override
  public Real exp(Real rhs)
  {
    return new Real(Math.pow(this.value, rhs.value));
  }
  
  @Override
  public Complex exp(Complex rhs)
  {
    return toComplex().exp(rhs);
  }
    
  @Override
  public Real log(Real rhs)
  {
    // log_b (x) = ln (x) / ln (b)
    return new Real(Math.log(this.value) / Math.log(rhs.value));
  }
  
  @Override
  public Complex log(Complex rhs)
  {
    return toComplex().log(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Int rhs)  
  {
    return isEqual(rhs.toReal());
  }
  
  @Override // IEq
  public boolean isEqual(BigInt rhs)
  {
    return isEqual(rhs.toReal());
  }
  
  @Override // IEq
  public boolean isEqual(Ratio rhs)  
  {
    return isEqual(rhs.toReal());
  }
  
  @Override // IEq
  public boolean isEqual(Real rhs)
  {
    return this.value == rhs.value;
  }
  
  @Override // IEq
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }
  
  @Override // IOrd
  public int compareTo(Int rhs)
  {
    return compareTo(rhs.toReal());
  }
  
  @Override // IOrd
  public int compareTo(BigInt rhs)
  {
    return compareTo(rhs.toReal());
  }
  
  @Override // IOrd
  public int compareTo(Ratio rhs)
  {
    return compareTo(rhs.toReal());
  }
  
  @Override // IOrd
  public int compareTo(Real that)
  {
    return Double.compare(this.value, that.value);
  }
  
  @Override
  public IReal real()
  {
    return new Real(value);
  }
  
  @Override
  public IReal imag()
  {
    return ZERO;
  }
  
  @Override
  public Real angle()
  {
    return toComplex().angle();
  }
  
  @Override
  public Real magnitude()
  {
    return toComplex().magnitude();
  }
  
  private final double value;
}
