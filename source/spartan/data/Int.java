package spartan.data;

public final class Int extends Datum
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
  
  public Int neg()
  {
    return new Int(-value);
  }
  
  public Int abs()
  {
    return new Int(Math.abs(value));
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
  
  public static Int mod(Int x, Int y)
  {
    return new Int(x.value % y.value);
  }
  
  public static boolean eq(Int x, Int y)
  {
    return x.value == y.value;
  }
  
  public static int compare(Int x, Int y)
  {
    return Integer.compare(x.value, y.value);
  }

  final int value;
}
