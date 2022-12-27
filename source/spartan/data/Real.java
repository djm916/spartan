package spartan.data;

import spartan.errors.IntegerOverflow;

public final class Real implements Datum, Numeric, IEq<Real>, IOrd<Real>
{
  public static final Real PI = new Real(Math.PI);
  public static final Real E = new Real(Math.E);
  public static final Real POS_INF = new Real(Double.POSITIVE_INFINITY);
  public static final Real NEG_INF = new Real(Double.NEGATIVE_INFINITY);
  public static final Real NAN = new Real(Double.NaN);
  
  public Real(double value)
  {
    this.value = value;
  }
  
  public Real(String val)
  {
    this(Double.parseDouble(val));
  }
  
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
  
  public Int toInt()
  {
    if (value < Integer.MIN_VALUE || value > Integer.MAX_VALUE)
      throw new IntegerOverflow();
    return new Int((int)value);
  }
  
  public Complex toComplex()
  {
    return new Complex(value, 0.0);
  }
  
  public Real neg()
  {
    return new Real(-value);
  }
  
  public Real abs()
  {
    return new Real(Math.abs(value));
  }
  
  public Real floor()
  {
    return new Real(Math.floor(value));
  }
  
  public Real ceiling()
  {
    return new Real(Math.ceil(value));
  }
  
  public Real round()
  {
    return new Real(Math.round(value));
  }
    
  public Real exp(Real that)
  {
    return new Real(Math.pow(this.value, that.value));
  }
  
  public Real log(Real that)
  {
    // log_b (x) = ln (x) / ln (b)
    
    return new Real(Math.log(this.value) / Math.log(that.value));
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
  
  public Real add(Real that)
  {
    return new Real(this.value + that.value);
  }
  
  public Real sub(Real that)
  {
    return new Real(this.value - that.value);
  }
  
  public Real mul(Real that)
  {
    return new Real(this.value * that.value);
  }
  
  public Real div(Real that)
  {
    return new Real(this.value / that.value);
  }
  
  @Override // IEq
  public boolean isEqual(Real that)
  {
    return this.value == that.value;
  }
  
  @Override // IOrd
  public int compareTo(Real that)
  {
    return Double.compare(this.value, that.value);
  }
  
  public final double value;
}
