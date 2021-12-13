package spartan.data;

import java.math.BigInteger;

public final class Ratio extends Datum
{
  public Ratio(String numer, String denom)
  {
    this.numer = new BigInteger(numer);
    this.denom = new BigInteger(denom);
    reduce();
  }
  
  public Ratio(Int numer, Int denom)
  {
    this(numer.value(), denom.value());
  }
  
  public Ratio(BigInteger numer, BigInteger denom)
  {
    this.numer = numer;
    this.denom = denom;
    reduce();
  }
  
  public Type type()
  {
    return Type.Ratio;
  }
  
  public String repr()
  {
    return String.format("%s/%s", numer, denom);
  }
  
  public Ratio abs()
  {
    return new Ratio(numer.abs(), denom.abs());
  }
  
  public Ratio neg()
  {
    return new Ratio(numer.negate(), denom);
  }
  
  /* Add two rational numbers
  
     let x = a/b, y = c/d
     
     z = x + y = (a/b)*(d/d) + (c/d)*(b/b)
               = (a*d)/(b*d) + (c*b)/(d*b)
               = ((a*d)+(c*b))/(b*d)
  */
  public static Ratio add(Ratio x, Ratio y)
  {
    return new Ratio(x.numer.multiply(y.denom).add(y.numer.multiply(x.denom)),
                     x.denom.multiply(y.denom));
  }
  
  /* Subtract two rational numbers
  
     let x = a/b, y = c/d
     
     z = x - y = (a/b)*(d/d) - (c/d)*(b/b)
               = (a*d)/(b*d) - (c*b)/(d*b)
               = ((a*d)-(c*b))/(b*d)
  */
  public static Ratio sub(Ratio x, Ratio y)
  {
    return new Ratio(x.numer.multiply(y.denom).subtract(y.numer.multiply(x.denom)),
                     x.denom.multiply(y.denom));
  }
  
  /* Multiply two rational numbers
  
     let x = a/b, y = c/d
     
     z = x * y = (a/b)*(c/d) = (a*c)/(b*d)
  */
  public static Ratio mul(Ratio x, Ratio y)
  {
    return new Ratio(x.numer.multiply(y.numer), x.denom.multiply(y.denom));
  }
  
  /* Multiply two rational numbers
  
     let x = a/b, y = c/d
     
     z = x / y = x * (1/y) = (a/b)*(d/c) = (a*d)/(b*c)
  */
  public static Ratio div(Ratio x, Ratio y)
  {
    return new Ratio(x.numer.multiply(y.denom), x.denom.multiply(y.numer));
  }
  
  private void reduce()
  {
    var gcd = numer.gcd(denom);
    numer = numer.divide(gcd);
    denom = denom.divide(gcd);
  }
  
  private BigInteger numer;
  private BigInteger denom;
}
