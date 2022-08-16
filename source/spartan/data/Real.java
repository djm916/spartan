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
  
  public long longValue()
  {
    return (long)value;
  }
  
  public double doubleValue()
  {
    return value;
  }
  
  public String repr()
  {
    return Double.toString(value);
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
  
  public Int round()
  {
    return new Int(Math.round(value));
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
  
  public Real add(Real other)
  {
    return new Real(this.value + other.value);
  }
  
  public Real sub(Real other)
  {
    return new Real(this.value - other.value);
  }
  
  public Real mul(Real other)
  {
    return new Real(this.value * other.value);
  }
  
  public Real div(Real other)
  {
    return new Real(this.value / other.value);
  }
    
  public boolean eq(Real other)
  {
    return this.value == other.value;
  }
  
  public int compare(Real other)
  {
    return Double.compare(this.value, other.value);
  }
  
  private final double value;
}
