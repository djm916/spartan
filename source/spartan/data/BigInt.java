package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class BigInt implements Datum, INum, IInt, IRatio, IReal, IComplex, IEq, IOrd
{
  public static final BigInt ZERO = new BigInt(0);
  public static final BigInt ONE = new BigInt(1);
  
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
  public String repr()
  {
    return value.toString();
  }
  
  @Override
  public int toInt32()
  {
    try {
      return value.intValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
  
  @Override
  public long toInt64()
  {
    try {
      return value.longValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
/*  
  @Override 
  public BigInteger bigIntValue()
  {
    return value;
  }
*/
  @Override // INum
  public double toFloat64()
  {
    return value.doubleValue();
  }
  
  @Override
  public String format(int base)
  {
    return value.toString(base);
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
  public boolean isEqual(Int rhs)  
  {
    return isEqual(rhs.toBigInt());
  }
  
  @Override // IEq
  public boolean isEqual(BigInt rhs)
  {
    return this.value.equals(rhs.value);
  }
  
  @Override // IEq
  public boolean isEqual(Ratio rhs)  
  {
    return toRatio().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Real rhs)  
  {
    return toReal().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }
  
  @Override // IOrd
  public int compareTo(Int rhs)
  {
    return compareTo(rhs.toBigInt());
  }
  
  @Override // IOrd
  public int compareTo(BigInt rhs)
  {
    return this.value.compareTo(rhs.value);
  }
  
  @Override // IOrd
  public int compareTo(Ratio rhs)
  {
    return toRatio().compareTo(rhs);
  }
  
  @Override // IOrd
  public int compareTo(Real rhs)
  {
    return toReal().compareTo(rhs);
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
  public Ratio over(Int rhs)
  {
    return over(rhs.toBigInt());
  }
  
  @Override
  public Ratio over(BigInt rhs)
  {
    return new Ratio(this.value, rhs.value);
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
  
  @Override
  public Real realPart()
  {
    return toReal();
  }
  
  @Override
  public Real imagPart()
  {
    return Real.ZERO;
  }
  
  @Override
  public Real angle()
  {
    return toComplex().angle();
  }
  
  @Override
  public Real magnitude()
  {
    return toComplex().magnitude();
  }
  
  @Override
  public BigInt numerator()
  {
    return this;
  }
  
  @Override
  public BigInt denominator()
  {
    return ONE;
  }
  
  private final BigInteger value;
}
