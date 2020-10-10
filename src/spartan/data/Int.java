package spartan.data;

public class Int extends Value
{
  public Int(int value)
  {
    this.value = value;
  }
  
  public Type type()
  {
    return Type.Int;
  }
  
  public String repr()
  {
    return Integer.toString(value);
  }
  
  public static Int add(Int x, Int y)
  {
    return new Int(x.value + y.value);
  }
  
  public static Int sub(Int x, Int y)
  {
    return new Int(x.value - y.value);
  }
  
  public static Int mul(Int x, Int y)
  {
    return new Int(x.value * y.value);
  }
  
  public static Int div(Int x, Int y)
  {
    return new Int(x.value / y.value);
  }
  
  public static boolean eq(Int x, Int y)
  {
    return x.value == y.value;
  }
  
  private final int value;
}
