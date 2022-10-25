package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class BigInt implements Datum, Integral
{
  public BigInt(long value)
  {
    this(BigInteger.valueOf(value));
  }
  
  public BigInt(BigInteger value)
  {
    this.value = value;
  }
  
  public BigInt(String value)
  {
    this(new BigInteger(value));
  }
  
  @Override
  public Type type()
  {
    return Type.BIGINT;
  }
  
  @Override
  public String repr()
  {
    return value.toString();
  }
  
  @Override
  public int intValue()
  {
    try {
      return value.intValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
  
  @Override
  public boolean equals(Object other)
  {
    return (other instanceof BigInt that) && this.value.equals(that.value);
  }
  
  @Override
  public int hashCode()
  {
    return value.hashCode();
  }
  
  public Ratio toRatio()
  {
    return new Ratio(value, BigInteger.ONE);
  }
  
  public Real toReal()
  {
    return new Real(value.doubleValue());
  }
  
  public Complex toComplex()
  {
    return new Complex(value.doubleValue(), 0.0);
  }
  
  public BigInt neg()
  {
    return new BigInt(value.negate());
  }
  
  public BigInt abs()
  {
    return new BigInt(value.abs());
  }
  
  public BigInt floor()
  {
    return this;
  }
  
  public BigInt ceiling()
  {
    return this;
  }
  
  public BigInt truncate()
  {
    return this;
  }
  
  public BigInt round()
  {
    return this;
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
    
  public Ratio div(BigInt other)
  {
    try {
      return new Ratio(this.value, other.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public BigInt quotient(BigInt other)
  {
    try {
      return new BigInt(this.value.divide(other.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public BigInt remainder(BigInt other)
  {
    try {
      return new BigInt(this.value.remainder(other.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public boolean eq(BigInt other)
  {
    return this.value.equals(other.value);
  }
  
  public int compare(BigInt other)
  {
    return this.value.compareTo(other.value);
  }
  
  public final BigInteger value;
}
