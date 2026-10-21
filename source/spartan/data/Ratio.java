package spartan.data;

import java.math.BigInteger;
import spartan.errors.InvalidArgument;

public final class Ratio implements Datum, INum, IRatio, IReal, IComplex, IEq, IOrd
{
  public static Ratio ZERO = new Ratio(0, 1);
  
  public Ratio(BigInteger numer, BigInteger denom)
  {
    if (denom.equals(BigInteger.ZERO))
      throw new InvalidArgument();
    
    // Maintain the invariant that the numerator carries the sign
    // of the fraction, and the denominator is always positive.
    if (denom.compareTo(BigInteger.ZERO) < 0) {
      numer = numer.negate();
      denom = denom.negate();
    }
    
    // Maintain the invariant that fractions are reduced to lowest terms,
    // such that the numerator and denominator have no common factors.
    var gcd = numer.gcd(denom);    
    numer = numer.divide(gcd);
    denom = denom.divide(gcd);
    
    this.numer = numer;
    this.denom = denom;
  }
  
  public Ratio(String numer, String denom)
  {
    this(new BigInteger(numer), new BigInteger(denom));
  }
  
  public Ratio(long numer, long denom)
  {
    this(BigInteger.valueOf(numer), BigInteger.valueOf(denom));
  }
  
  @Override
  public Type type()
  {
    return Type.RATNUM;
  }
  
  @Override
  public String repr()
  {
    return String.format("%s/%s", numer, denom);
  }
  
  @Override
  public String formatDec(int precision)
  {
    return toFloat64().formatDec(precision);
  }
  
  @Override // IRatio
  public BigInt numerator()
  {
    return new BigInt(numer);
  }
  
  @Override // IRatio
  public BigInt denominator()
  {
    return new BigInt(denom);
  }
  
  @Override
  public double doubleValue()
  {
    return numer.doubleValue() / denom.doubleValue();
  }
  
  public Float64 toFloat64()
  {
    return new Float64(doubleValue());
  }
  
  public Complex toComplex()
  {
    return new Complex(doubleValue(), 0.0);
  }
  
  @Override
  public Ratio abs()
  {
    return new Ratio(numer.abs(), denom.abs());
  }
  
  @Override
  public Ratio neg()
  {
    return new Ratio(numer.negate(), denom);
  }
  
  @Override
  public Float64 floor()
  {
    return new Float64(Math.floor(doubleValue()));
  }
  
  @Override
  public Float64 ceiling()
  {
    return new Float64(Math.ceil(doubleValue()));
  }
  
  @Override
  public Float64 round()
  {
    return new Float64(Math.round(doubleValue()));
  }
  
  @Override
  public Ratio add(Int rhs)
  {
    return add(rhs.toRatio());
  }
  
  @Override
  public Ratio add(BigInt rhs)
  {
    return add(rhs.toRatio());
  }
  
  /* Add two rational numbers
  
     let x = a/b, y = c/d
     
     x + y = (a/b)*(d/d) + (c/d)*(b/b)
           = (a*d)/(b*d) + (c*b)/(d*b)
           = ((a*d)+(c*b))/(b*d)
  */
  @Override
  public Ratio add(Ratio other)
  {
    var a = this.numer;
    var b = this.denom;
    var c = other.numer;
    var d = other.denom;
    return new Ratio(a.multiply(d).add(c.multiply(b)), b.multiply(d));
  }
  
  @Override
  public Float64 add(Float64 rhs)
  {
    return toFloat64().add(rhs);
  }
  
  @Override
  public Complex add(Complex rhs)
  {
    return toComplex().add(rhs);
  }

  @Override
  public Ratio sub(Int rhs)
  {
    return sub(rhs.toRatio());
  }
  
  @Override
  public Ratio sub(BigInt rhs)
  {
    return sub(rhs.toRatio());
  }
  
  /* Subtract two rational numbers
  
     let x = a/b, y = c/d
     
     x - y = x + (-y)
  */
  @Override
  public Ratio sub(Ratio other)
  {
    var a = this.numer;
    var b = this.denom;
    var c = other.numer;
    var d = other.denom;
    return new Ratio(a.multiply(d).subtract(c.multiply(b)), b.multiply(d));
  }
  
  @Override
  public Float64 sub(Float64 rhs)
  {
    return toFloat64().sub(rhs);
  }
  
  @Override
  public Complex sub(Complex rhs)
  {
    return toComplex().sub(rhs);
  }

  @Override
  public Ratio mul(Int rhs)
  {
    return mul(rhs.toRatio());
  }
  
  @Override
  public Ratio mul(BigInt rhs)
  {
    return mul(rhs.toRatio());
  }
  
  /* Multiply two rational numbers
  
     let x = a/b, y = c/d
     
     x * y = (a/b)*(c/d) = (a*c)/(b*d)
  */
  public Ratio mul(Ratio other)
  {
    var a = this.numer;
    var b = this.denom;
    var c = other.numer;
    var d = other.denom;
    return new Ratio(a.multiply(c), b.multiply(d));
  }

  @Override
  public Float64 mul(Float64 rhs)
  {
    return toFloat64().mul(rhs);
  }
  
  @Override
  public Complex mul(Complex rhs)
  {
    return toComplex().mul(rhs);
  }

  @Override
  public Ratio div(Int rhs)
  {
    return div(rhs.toRatio());
  }
  
  @Override
  public Ratio div(BigInt rhs)
  {
    return div(rhs.toRatio());
  }
  
  /* Divide two rational numbers
  
     let x = a/b, y = c/d
     
     x / y = x * (1/y) = (a*d)/(b*c)
  */
  @Override
  public Ratio div(Ratio rhs)
  {
    var a = this.numer;
    var b = this.denom;
    var c = rhs.numer;
    var d = rhs.denom;
    return new Ratio(a.multiply(d), b.multiply(c));
  }
  
  @Override
  public Float64 div(Float64 rhs)
  {
    return toFloat64().div(rhs);
  }
  
  @Override
  public Complex div(Complex rhs)
  {
    return toComplex().div(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Int rhs)  
  {
    return isEqual(rhs.toRatio());
  }
  
  @Override // IEq
  public boolean isEqual(BigInt rhs)
  {
    return isEqual(rhs.toRatio());
  }
  
  /* Equivalence predicate for rational numbers.     
  
     let x = a/b, y = c/d
     
     x = y iff b = d ^ a = c
  */
  
  @Override // IEq
  public boolean isEqual(Ratio rhs)
  {
    var a = this.numer;
    var b = this.denom;
    var c = rhs.numer;
    var d = rhs.denom;
    return b.equals(d) && a.equals(c);
  }
  
  @Override // IEq
  public boolean isEqual(Float64 rhs)  
  {
    return toFloat64().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }
  
  @Override // IEq
  public int compareTo(Int rhs)
  {
    return compareTo(rhs.toRatio());
  }
  
  @Override // IEq
  public int compareTo(BigInt rhs)
  {
    return compareTo(rhs.toRatio());
  }
  
  /* Ordering function for rational numbers.     
  
     let x = a/b, y = c/d
     
     x < y => (d/d) * (a/b) < (b/b) * (c/d)
           => (a*d)/(b*d) < (b*c)/(b*d)
           => (a*d) < (b*c)
  */
  @Override // IEq
  public int compareTo(Ratio rhs)
  {
    var a = this.numer;
    var b = this.denom;
    var c = rhs.numer;
    var d = rhs.denom;
    
    // Same denominator, compare by numerator
    if (b.equals(d))
      return a.compareTo(c);
    
    return a.multiply(d).compareTo(b.multiply(c));
  }
  
  @Override
  public int compareTo(Float64 rhs)
  {
    return toFloat64().compareTo(rhs);
  }
  
  @Override // IComplex
  public Float64 realPart()
  {
    return toFloat64();
  }
  
  @Override // IComplex
  public Int imagPart()
  {
    return Int.ZERO;
  }
  
  @Override // IComplex
  public Float64 angle()
  {
    return toComplex().angle();
  }
  
  @Override // IComplex
  public Float64 magnitude()
  {
    return toComplex().magnitude();
  }
  
  @Override // INum
  public boolean isFinite()
  {
    return true;
  }
  
  @Override // INum
  public boolean isNaN()
  {
    return false;
  }
  
  @Override // INum
  public boolean isInteger()
  {
    return denom.equals(BigInteger.ONE);
  }
  
  @Override // INum
  public boolean isReal()
  {
    return true;
  }
  
  @Override // INum
  public boolean isRational()
  {
    return true;
  }
  
  @Override // INum
  public boolean isComplex()
  {
    return true;
  }
  
  @Override // INum
  public boolean isZero()
  {
    // In this representation, the denominator is never 0 and the zero
    // value has zero numerator and 1 denominator.
    return numer.equals(BigInteger.ZERO);
  }
  
  @Override // IReal
  public boolean isPositive()
  {
    // In this representation, the numerator always carries the sign
    return numer.compareTo(BigInteger.ZERO) > 0;
  }
  
  @Override // IReal
  public boolean isNegative()
  {
    // In this representation, the numerator always carries the sign
    return numer.compareTo(BigInteger.ZERO) < 0;
  }
  
  @Override // INum
  public boolean isExact()
  {
    return true;
  }
  
  @Override // INum
  public Ratio toExact()
  {
    return this;
  }
  
  @Override // INum
  public INum toInexact()
  {
    return toFloat64();
  }
  
  @Override // IReal
  public IRatio rationalize()
  {
    return this;
  }
  
  private final BigInteger numer;
  private final BigInteger denom;
}
