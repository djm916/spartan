package spartan.data;

public final class Real extends Datum
{
  public static final Real PI = new Real(Math.PI);
  public static final Real E = new Real(Math.E);
  
  public Real(double value)
  {
    this.value = value;
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
  
  public Real exp(Real that)
  {
    return new Real(Math.pow(this.value, that.value));
  }
  
  public Real log(Real that)
  {
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
  
  public boolean eq(Real that)
  {
    return this.value == that.value;
  }
  
  public int compare(Real that)
  {
    return Double.compare(this.value, that.value);
  }
  
  private final double value;
}
