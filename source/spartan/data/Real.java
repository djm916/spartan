package spartan.data;

public final class Real extends Datum
{
  public static final Real PI = new Real(Math.PI);
  public static final Real E = new Real(Math.E);
  
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
    return Type.Real;
  }
  
  public String repr()
  {
    return String.format("%g", value);
  }
  
  public double value()
  {
    return value;
  }
  
  public long longValue()
  {
    return (long) value;
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
  
  public Real ceil()
  {
    return new Real(Math.ceil(value));
  }
    
  public static Real exp(Real base, Real pow)
  {
    return new Real(Math.pow(base.value, pow.value));
  }
  
  public static Real log(Real base, Real arg)
  {
    return new Real(Math.log(arg.value) / Math.log(base.value));
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
  
  public static Real add(Real x, Real y)
  {
    return new Real(x.value + y.value);
  }
  
  public static Real sub(Real x, Real y)
  {
    return new Real(x.value - y.value);
  }
  
  public static Real mul(Real x, Real y)
  {
    return new Real(x.value * y.value);
  }
  
  public static Real div(Real x, Real y)
  {
    return new Real(x.value / y.value);
  }
    
  public static boolean eq(Real x, Real y)
  {
    return x.value == y.value;
  }
  
  public static int compare(Real x, Real y)
  {
    return Double.compare(x.value, y.value);
  }
  
  private final double value;
}
