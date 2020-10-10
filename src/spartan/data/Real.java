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
  
  private final double value;
}
