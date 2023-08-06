package spartan.data;

import java.math.BigInteger;
import spartan.errors.InvalidArgument;

public final class Ratio implements Datum, INum, IRatio, IReal, IComplex, IEq, IOrd
{
  public Ratio(BigInteger numer, BigInteger denom)
  {
    if (denom.equals(BigInteger.ZERO))
      throw new InvalidArgument();
    
    // Maintain the invariant that the numerator carries the sign,
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
    this(BigInteger.valueOf(numer),
         BigInteger.valueOf(denom));
  }
  
  @Override
  public String type()
  {
    return "rational";
  }
  
  @Override
  public String repr()
  {
    return String.format("%s/%s", numer, denom);
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
  public double toFloat64()
  {
    return numer.doubleValue() / denom.doubleValue();
  }
  
  public Real toReal()
  {
    return new Real(toFloat64());
  }
  
  public Complex toComplex()
  {
    return new Complex(toFloat64(), 0.0);
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
  public Int floor()
  {
    return Int.valueOf((long) Math.floor(toFloat64()));
  }
  
  @Override
  public Int ceiling()
  {
    return Int.valueOf((long) Math.ceil(toFloat64()));
  }
  
  public Int truncate()
  {
    return Int.valueOf((int) toFloat64());
  }
  
  @Override
  public Int round()
  {
    return Int.valueOf((long) Math.round(toFloat64()));
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
  public Real div(Real rhs)
  {
    return toReal().div(rhs);
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
  public boolean isEqual(Real rhs)  
  {
    return toReal().isEqual(rhs);
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
  public int compareTo(Real rhs)
  {
    return toReal().compareTo(rhs);
  }
  
  @Override
  public Real real()
  {
    return toReal();
  }
  
  @Override
  public Real imag()
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
  
  private final BigInteger numer;
  private final BigInteger denom;
}
