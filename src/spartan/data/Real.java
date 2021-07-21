package spartan.data;

public class Real extends Datum
{
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
    return Double.toString(value);
  }
  
  public final Real neg()
  {
    return new Real(-value);
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
