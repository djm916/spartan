package spartan.data;

import java.math.BigInteger;

public class BigInt extends Datum
{
  public BigInt(int val)
  {
    this.value = BigInteger.valueOf(val);
  }
  
  public BigInt(String val)
  {
    this.value = new BigInteger(val);
  }
  
  public Type type()
  {
    return Type.BIGINT;
  }
  
  public String repr()
  {
    return value.toString();
  }
  
  public BigInt add(BigInt other)
  {
    return this.value.add(other.value);
  }
  
  public BigInt sub(BigInt other)
  {
    return this.value.subtract(other.value);
  }
  
  public BigInt mul(BigInt other)
  {
    return this.value.multiply(other.value);
  }
  
  public BigInt div(BigInt other)
  {
    return null;
  }
  
  private final BigInteger value;
}
