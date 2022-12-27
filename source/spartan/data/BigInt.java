package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class BigInt implements Datum, Integral, IEq<BigInt>, IOrd<BigInt>
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
  
  @Override // Datum
  public Type type()
  {
    return Type.BIGINT;
  }
  
  @Override // Datum
  public String repr()
  {
    return value.toString();
  }
  
  @Override // Integral
  public int intValue()
  {
    try {
      return value.intValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
  
  @Override // Object
  public boolean equals(Object other)
  {
    return (other instanceof BigInt that) && this.value.equals(that.value);
  }
  
  @Override // Object
  public int hashCode()
  {
    return value.hashCode();
  }
  
  @Override // IEq
  public boolean isEqual(BigInt that)
  {
    return this.value.equals(that.value);
  }
  
  @Override // IOrd
  public int compareTo(BigInt that)
  {
    return this.value.compareTo(that.value);
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
    
  public BigInt add(BigInt that)
  {
    return new BigInt(this.value.add(that.value));
  }
  
  public BigInt sub(BigInt that)
  {
    return new BigInt(this.value.subtract(that.value));
  }
  
  public BigInt mul(BigInt that)
  {
    return new BigInt(this.value.multiply(that.value));
  }
    
  public Ratio div(BigInt that)
  {
    try {
      return new Ratio(this.value, that.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public BigInt quotient(BigInt that)
  {
    try {
      return new BigInt(this.value.divide(that.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public BigInt remainder(BigInt that)
  {
    try {
      return new BigInt(this.value.remainder(that.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public final BigInteger value;
}
