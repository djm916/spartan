package spartan.data;

public class Real extends Value
{
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
    return Double.toString(value);
  }
  
  public Real neg()
  {
    return new Real(-value);
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
