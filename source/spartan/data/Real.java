package spartan.data;

public class Real extends Datum
{
  public static final Real PI = new Real(Math.PI);
  public static final Real E = new Real(Math.E);
  
  public Real(double value)
  {
    this.value = value;
  }
  
  public final Type type()
  {
    return Type.Real;
  }
  
  public final String repr()
  {
    //return String.format("%.3f", value);
    return Double.toString(value);
  }
  
  public final Real neg()
  {
    return new Real(-value);
  }
  
  public final Real abs()
  {
    return new Real(Math.abs(value));
  }
  
  public final Real floor()
  {
    return new Real(Math.floor(value));
  }
  
  public final Real ceil()
  {
    return new Real(Math.ceil(value));
  }
  
  public final Real exp(Real that)
  {
    return new Real(Math.pow(this.value, that.value));
  }
  
  public final Real log(Real that)
  {
    return new Real(Math.log(this.value) / Math.log(that.value));
  }
  
  public final Real sin()
  {
    return new Real(Math.sin(value));
  }
  
  public final Real cos()
  {
    return new Real(Math.cos(value));
  }
  
  public final Real tan()
  {
    return new Real(Math.tan(value));
  }
  
  public final Real asin()
  {
    return new Real(Math.asin(value));
  }
  
  public final Real acos()
  {
    return new Real(Math.acos(value));
  }
  
  public final Real atan()
  {
    return new Real(Math.atan(value));
  }
  
  public final Real add(Real that)
  {
    return new Real(this.value + that.value);
  }
  
  public final Real sub(Real that)
  {
    return new Real(this.value - that.value);
  }
  
  public final Real mul(Real that)
  {
    return new Real(this.value * that.value);
  }
  
  public final Real div(Real that)
  {
    return new Real(this.value / that.value);
  }
  
  public final boolean eq(Real that)
  {
    return this.value == that.value;
  }
  
  public final int compare(Real that)
  {
    return Double.compare(this.value, that.value);
  }
  
  private final double value;
}
