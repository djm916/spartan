package spartan.data;

import java.math.BigInteger;
import spartan.errors.InvalidArgument;

public final class Ratio implements Datum, INum, IReal, IEq, IOrd
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
  
    
  public Ratio(int numer, int denom)
  {
    this(BigInteger.valueOf(numer),
         BigInteger.valueOf(denom));
  }
  
  public Type type()
  {
    return Type.RATIO;
  }
  
  public String repr()
  {
    return String.format("%s/%s", numer, denom);
  }
  
  public BigInt numerator()
  {
    return new BigInt(numer);
  }
  
  public BigInt denominator()
  {
    return new BigInt(denom);
  }
  
  public double approx()
  {
    return numer.doubleValue() / denom.doubleValue();
  }
  
  @Override
  public double doubleValue()
  {
    return approx();
  }
  
  public Real toReal()
  {
    return new Real(approx());
  }
  
  public Complex toComplex()
  {
    return new Complex(approx(), 0.0);
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
    return new Int((int) Math.floor(approx()));
  }
  
  @Override
  public Int ceiling()
  {
    return new Int((int) Math.ceil(approx()));
  }
  
  
  public Int truncate()
  {
    return new Int((int) approx());
  }
  
  @Override
  public Int round()
  {
    return new Int((int) Math.round(approx()));
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
  
  private final BigInteger numer;
  private final BigInteger denom;
}
