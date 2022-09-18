package spartan.data;

import java.math.BigInteger;

public class BigInt extends IntBase
{
  public BigInt(int value)
  {
    this.value = BigInteger.valueOf(value);
  }
  
  public BigInt(String value)
  {
    this.value = new BigInteger(value);
  }
  
  public BigInt(BigInteger value)
  {
    this.value = value;
  }
  
  public Type type()
  {
    return Type.BIGINT;
  }
  
  public String repr()
  {
    return value.toString();
  }
  
  public BigInt neg()
  {
    return new BigInt(value.negate());
  }
  
  public BigInt add(BigInt other)
  {
    return new BigInt(this.value.add(other.value));
  }
  
  public BigInt sub(BigInt other)
  {
    return new BigInt(this.value.subtract(other.value));
  }
  
  public BigInt mul(BigInt other)
  {
    return new BigInt(this.value.multiply(other.value));
  }
  
  public BigInt div(BigInt other)
  {
    return null;
  }
  
  private final BigInteger value;
}
