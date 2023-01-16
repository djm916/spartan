package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class BigInt implements Datum, INum, IInt, IReal, IEq<BigInt>, IOrd<BigInt>
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
  public double doubleValue()
  {
    return value.doubleValue();
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
  
  @Override
  public BigInt neg()
  {
    return new BigInt(value.negate());
  }
  
  @Override
  public BigInt abs()
  {
    return new BigInt(value.abs());
  }
  
  @Override
  public BigInt add(Int rhs)
  {
    return add(rhs.toBigInt());
  }
  
  @Override
  public BigInt add(BigInt rhs)
  {
    return new BigInt(this.value.add(rhs.value));
  }
  
  @Override
  public Ratio add(Ratio rhs)
  {
    return toRatio().add(rhs);
  }
  
  @Override
  public Real add(Real rhs)
  {
    return toReal().add(rhs);
  }
  
  @Override
  public Complex add(Complex rhs)
  {
    return toComplex().add(rhs);
  }
  
  @Override
  public BigInt sub(Int rhs)
  {
    return sub(rhs.toBigInt());
  }
  
  @Override
  public BigInt sub(BigInt rhs)
  {
    return new BigInt(this.value.subtract(rhs.value));
  }
  
  @Override
  public Ratio sub(Ratio rhs)
  {
    return toRatio().sub(rhs);
  }
  
  @Override
  public Real sub(Real rhs)
  {
    return toReal().sub(rhs);
  }
  
  @Override
  public Complex sub(Complex rhs)
  {
    return toComplex().sub(rhs);
  }
  
  @Override
  public BigInt mul(Int rhs)
  {
    return mul(rhs.toBigInt());
  }
  
  @Override
  public BigInt mul(BigInt rhs)
  {
    return new BigInt(this.value.multiply(rhs.value));
  }
  
  @Override
  public Ratio mul(Ratio rhs)
  {
    return toRatio().mul(rhs);
  }
  
  @Override
  public Real mul(Real rhs)
  {
    return toReal().mul(rhs);
  }
  
  @Override
  public Complex mul(Complex rhs)
  {
    return toComplex().mul(rhs);
  }
  
  @Override
  public Ratio div(Int rhs)
  {
    return div(rhs.toBigInt());
  }
  
  @Override
  public Ratio div(BigInt rhs)
  {
    return new Ratio(this.value, rhs.value);
  }
  
  @Override
  public Ratio div(Ratio rhs)
  {
    return toRatio().div(rhs);
  }
  
  @Override
  public Real div(Real rhs)
  {
    return toReal().div(rhs);
  }
  
  @Override
  public Complex div(Complex rhs)
  {
    return toComplex().div(rhs);
  }
  
  @Override
  public BigInt quotient(Int rhs)
  {
    return quotient(rhs.toBigInt());
  }
  
  @Override
  public BigInt quotient(BigInt rhs)
  {
    try {
      return new BigInt(this.value.divide(rhs.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  @Override
  public BigInt remainder(Int rhs)
  {
    return remainder(rhs.toBigInt());
  }
  
  @Override
  public BigInt remainder(BigInt rhs)
  {
    try {
      return new BigInt(this.value.remainder(rhs.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  @Override
  public Real sin()
  {
    return toReal().sin();
  }
    
  @Override
  public Real cos()
  {
    return toReal().cos();
  }
  
  @Override
  public Real tan()
  {
    return toReal().tan();
  }
  
  @Override
  public Real asin()
  {
    return toReal().asin();
  }
  
  @Override
  public Real acos()
  {
    return toReal().acos();
  }
  
  @Override
  public Real atan()
  {
    return toReal().atan();
  }
  
  @Override
  public Real exp(Int rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(BigInt rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(Ratio rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(Real rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Complex exp(Complex rhs)
  {
    return toComplex().exp(rhs);
  }
  
  @Override
  public Real log(Int rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(BigInt rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(Ratio rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(Real rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Complex log(Complex rhs)
  {
    return toComplex().log(rhs);
  }
  
  @Override
  public BigInt floor()
  {
    return this;
  }
  
  @Override
  public BigInt ceiling()
  {
    return this;
  }
  
  @Override
  public BigInt round()
  {
    return this;
  }
  
  private final BigInteger value;
}
