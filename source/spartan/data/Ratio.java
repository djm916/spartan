package spartan.data;

import java.math.BigInteger;
import spartan.errors.InvalidArgument;

public final class Ratio extends Datum
{
  public Ratio(BigInteger numer, BigInteger denom)
  {
    //if (denom.equals(BigInteger.ZERO))
      //throw new InvalidArgument();  
    
    if (denom.compareTo(BigInteger.ZERO) < 0) {
      numer = numer.negate();
      denom = denom.negate();
    }
    
    this.numer = numer;
    this.denom = denom;
    
    reduce();    
  }
  
  public Ratio(String numer, String denom)
  {
    this(new BigInteger(numer), new BigInteger(denom));
  }
  
  public Ratio(Int numer, Int denom)
  {
    this(numer.value(), denom.value());
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
  
  /* Divide two rational numbers
  
     let x = a/b, y = c/d
     
     z = x / y = x * (1/y) = (a/b)*(d/c) = (a*d)/(b*c)
  */
  public static Ratio div(Ratio x, Ratio y)
  {
    return new Ratio(x.numer.multiply(y.denom), x.denom.multiply(y.numer));
  }
  
  public static boolean eq(Ratio x, Ratio y)
  {
    return x.numer.equals(y.numer) && x.denom.equals(y.denom);
  }
  
  public static int compare(Ratio x, Ratio y)
  {    
    if (x.denom.equals(y.denom))
      return x.numer.compareTo(y.numer);
    
    var lhs = x.numer.multiply(y.denom);
    var rhs = x.denom.multiply(y.numer);
    return lhs.compareTo(rhs);
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
