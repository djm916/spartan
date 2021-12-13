package spartan.data;

import java.math.BigInteger;

public final class Int extends Datum
{
  public Int(int value)
  {
    this.value = BigInteger.valueOf(value);
  }
  
  public Int(String val)
  {
    this.value = new BigInteger(val);
  }
  
  public Type type()
  {
    return Type.Int;
  }
  
  public String repr()
  {
    return value.toString();
  }
  
  public BigInteger value()
  {
    return value;
  }
  
  public int intValue()
  {
    return value.intValue();
  }
  
  public double doubleValue()
  {
    return value.doubleValue();
  }
  
  public Int neg()
  {
    return new Int(value.negate());
  }
  
  public Int abs()
  {
    return new Int(value.abs());
  }
  
  public static Int add(Int x, Int y)
  {
    return new Int(x.value.add(y.value));
  }
  
  public static Int sub(Int x, Int y)
  {
    return new Int(x.value.subtract(y.value));
  }
  
  public static Int mul(Int x, Int y)
  {
    return new Int(x.value.multiply(y.value));
  }
  
  public static Int div(Int x, Int y)
  {
    return new Int(x.value.divide(y.value));
  }
  
  public static Int mod(Int x, Int y)
  {
    return new Int(x.value.mod(y.value));
  }
  
  public static boolean eq(Int x, Int y)
  {
    return x.value.equals(y.value);
  }
  
  public static int compare(Int x, Int y)
  {
    return x.value.compareTo(y.value);
  }
  
  private Int(BigInteger value)
  {
    this.value = value;
  }
  
  private final BigInteger value;
}
