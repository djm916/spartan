package spartan.data;

import java.math.BigInteger;
import spartan.errors.InvalidArgument;

public final class Ratio extends Datum
{
  public Ratio(BigInteger numer, BigInteger denom)
  {
    //if (denom.equals(BigInteger.ZERO))
      //throw new InvalidArgument();  
    
    // Maintain the invariant that the numerator carries the sign of
    // the fraction, such that the denominator is always positive.
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
  
  public Ratio(Int numer, Int denom)
  {
    this(numer.value, denom.value);
  }
  
  public Type type()
  {
    return Type.Ratio;
  }
  
  public String repr()
  {
    return String.format("%s/%s", numer, denom);
  }
  
  public Int numerator()
  {
    return new Int(numer);
  }
  
  public Int denominator()
  {
    return new Int(denom);
  }
  
  /* Take the absolute value of a fraction */
  
  public Ratio abs()
  {
    return new Ratio(numer.abs(), denom.abs());
  }
  
  /* Negate a fraction */
  
  public Ratio neg()
  {
    return new Ratio(numer.negate(), denom);
  }
  
  /* Add two rational numbers
  
     let x = a/b, y = c/d
     
     x + y = (a/b)*(d/d) + (c/d)*(b/b)
           = (a*d)/(b*d) + (c*b)/(d*b)
           = ((a*d)+(c*b))/(b*d)
  */
  public static Ratio add(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    return new Ratio(a.multiply(d).add(c.multiply(b)), b.multiply(d));
  }
  
  /* Subtract two rational numbers
  
     let x = a/b, y = c/d
     
     x - y = x + (-y)
  */
  public static Ratio sub(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    return new Ratio(a.multiply(d).subtract(c.multiply(b)), b.multiply(d));
  }
  
  /* Multiply two rational numbers
  
     let x = a/b, y = c/d
     
     x * y = (a/b)*(c/d) = (a*c)/(b*d)
  */
  public static Ratio mul(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    return new Ratio(a.multiply(c), b.multiply(d));
  }
  
  /* Divide two rational numbers
  
     let x = a/b, y = c/d
     
     x / y = x * (1/y) = (a*d)/(b*c)
  */
  public static Ratio div(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    return new Ratio(a.multiply(d), b.multiply(c));
  }
  
  /* Equivalence predicate for rational numbers.     
  
     let x = a/b, y = c/d
     
     x = y iff b = d ^ a = c
  */
  public static boolean eq(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    return b.equals(d) && a.equals(c);
  }
  
  /* Ordering function for rational numbers.     
  
     let x = a/b, y = c/d
     
     x < y => (d/d) * (a/b) < (b/b) * (c/d)
           => (a*d)/(b*d) < (b*c)/(b*d)
           => (a*d) < (b*c)
  */
  public static int compare(Ratio x, Ratio y)
  {
    var a = x.numer;
    var b = x.denom;
    var c = y.numer;
    var d = y.denom;
    
    // Same denominator, compare by numerator
    if (b.equals(d))
      return a.compareTo(c);
    
    return a.multiply(d).compareTo(b.multiply(c));
  }
    
  private BigInteger numer;
  private BigInteger denom;
}
