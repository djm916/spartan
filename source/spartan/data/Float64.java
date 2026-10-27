package spartan.data;

import spartan.errors.IntegerOverflow;
import spartan.errors.InvalidArgument;
import java.math.BigDecimal;
import java.math.BigInteger;

public final class Float64 implements Datum, INum, IReal, IComplex, ITrans, IEq, IOrd
{
  public static final Float64 PI = new Float64(Math.PI);
  public static final Float64 E = new Float64(Math.E);
  public static final Float64 POS_INF = new Float64(Double.POSITIVE_INFINITY);
  public static final Float64 NEG_INF = new Float64(Double.NEGATIVE_INFINITY);
  public static final Float64 NAN = new Float64(Double.NaN);
  public static final Float64 ZERO = new Float64(0.0);
    
  public Float64(double value)
  {
    this.value = value;
  }
  
  public Float64(String s)
  {
    this(parseFloat64(s));
  }
  
  public static double parseFloat64(String s)
  {
    if (s.equals("+inf.0"))
      return Double.POSITIVE_INFINITY;
    if (s.equals("-inf.0"))
      return Double.NEGATIVE_INFINITY;
    if (s.equals("+nan.0"))
      return Double.NaN;
    if (s.equals("-nan.0"))
      return Double.NaN;
    return Double.parseDouble(s);
  }
  
  public static String toString(double value)
  {
    if (Double.isInfinite(value))
      return value > 0 ? "+inf.0" : "-inf.0";
    if (Double.isNaN(value))
      return "+nan.0";
    return String.format("%.6g", value);
  }
  
  @Override
  public Type type()
  {
    return Type.FLONUM;
  }
  
  @Override
  public String repr()
  {
    return toString(value);
  }
  
  private static double round(double x, int n)
  {
    double scale = Math.pow(10.0, n);
    double result = Math.floor(x * scale + 0.5) / scale;
    return result;
  }
  
  @Override // IReal
  public String formatDec(int precision)
  {
    return Double.toString(round(this.value, precision));
  }
  
  public Complex toComplex()
  {
    return new Complex(value, 0.0);
  }
  
  @Override // IReal
  public double doubleValue()
  {
    return value;
  }
  
  @Override
  public Float64 neg()
  {
    return new Float64(-value);
  }
  
  @Override
  public Float64 abs()
  {
    return new Float64(Math.abs(value));
  }
  
  @Override
  public Float64 floor()
  {
    return new Float64(Math.floor(value));
  }
  
  @Override
  public Float64 ceiling()
  {
    return new Float64(Math.ceil(value));
  }
  
  @Override
  public Float64 round()
  {
    // Use Math.rint here instead of Math.round to avoid possible loss of precision when converting to long
    return new Float64(Math.rint(value));
  }
  
  /* By Euler's identity,
       
       sin(x + y*I) = sin(x)*cosh(y) + cos(x)*sinh(y)*I
       
     When y is (exactly) 0,
     
       cosh(y) = 1
       sinh(y) = 0
       
     and the expression reduces to the (real) sin function.
  */
  public Float64 sin()
  {
    return new Float64(Math.sin(value));
  }
  
  /* By Euler's identity,
  
       cos(x + y*I) = cos(x)*cosh(y) - sin(x)*sinh(y)*I
     
     Similiarly to above, when y is exactly 0, the imaginary term reduces to
     exactly 0, and the whole expression reduces to the (real) cos function.
  */
  public Float64 cos()
  {
    return new Float64(Math.cos(value));
  }
  
  /* By Euler's identity,

                      sin(2*x) + sinh(2*y)*I
      tan(x + y*I) =  ----------------------
                       cos(2*x) + cosh(2*y)

     In case y = 0, sinh(2*y) = 0 and cosh(2*y) = 1. Then,

                         sin(2*x)
      tan(x + y*I) =  --------------
                       cos(2*x) + 1

     Using the double-angle identities for sin and cos,

      sin(2*x) = 2*sin(x)*cos(x)
      cos(2*x) = 2*cos(x)^2 - 1

     the expression reduces to the (real) tan function:

               sin(x)
      tan(x) = ------
               cos(x)
  */
  public Float64 tan()
  {
    return new Float64(Math.tan(value));
  }
  
  public Float64 asin()
  {
    return new Float64(Math.asin(value));
  }
  
  public Float64 acos()
  {
    return new Float64(Math.acos(value));
  }
  
  public Float64 atan()
  {
    return new Float64(Math.atan(value));
  }
  
  @Override
  public Float64 add(Int rhs)
  {
    return add(rhs.toFloat64());
  }
  
  @Override
  public Float64 add(BigInt rhs)
  {
    return add(rhs.toFloat64());
  }
  
  @Override
  public Float64 add(Ratio rhs)
  {
    return add(rhs.toFloat64());
  }
  
  @Override
  public Float64 add(Float64 that)
  {
    return new Float64(this.value + that.value);
  }
  
  @Override
  public Complex add(Complex rhs)
  {
    return toComplex().add(rhs);
  }
  
  @Override
  public Float64 sub(Int rhs)
  {
    return sub(rhs.toFloat64());
  }
  
  @Override
  public Float64 sub(BigInt rhs)
  {
    return sub(rhs.toFloat64());
  }
  
  @Override
  public Float64 sub(Ratio rhs)
  {
    return sub(rhs.toFloat64());
  }
  
  @Override
  public Float64 sub(Float64 that)
  {
    return new Float64(this.value - that.value);
  }
  
  @Override
  public Complex sub(Complex rhs)
  {
    return toComplex().sub(rhs);
  }
  
  @Override
  public Float64 mul(Int rhs)
  {
    return mul(rhs.toFloat64());
  }
  
  @Override
  public Float64 mul(BigInt rhs)
  {
    return mul(rhs.toFloat64());
  }
  
  @Override
  public Float64 mul(Ratio rhs)
  {
    return mul(rhs.toFloat64());
  }
  
  @Override
  public Float64 mul(Float64 that)
  {
    return new Float64(this.value * that.value);
  }
  
  @Override
  public Complex mul(Complex rhs)
  {
    return toComplex().mul(rhs);
  }
  
  @Override
  public Float64 div(Int rhs)
  {
    return div(rhs.toFloat64());
  }
  
  @Override
  public Float64 div(BigInt rhs)
  {
    return div(rhs.toFloat64());
  }
  
  @Override
  public Float64 div(Ratio rhs)
  {
    return div(rhs.toFloat64());
  }
  
  @Override
  public Float64 div(Float64 rhs)
  {
    return new Float64(this.value / rhs.value);
  }
  
  @Override
  public Complex div(Complex rhs)
  {
    return toComplex().div(rhs);
  }
  
  @Override
  public ITrans exp(Float64 rhs)
  {
    // The power function is defined in terms of the exponential and (natural) logarithm.
    //
    //   x^y = e^(y*ln(x))
    //
    // This produces a complex result if x < 0, and y is not an integer.
    if (this.value < 0)
      return Complex.cexp(this.value, 0.0, rhs.value, 0.0);
    return new Float64(Math.pow(this.value, rhs.value));
  }
  
  @Override
  public Complex exp(Complex rhs)
  {
    return toComplex().exp(rhs);
  }
  
  @Override
  public Float64 log(Float64 rhs)
  {
    // log_b (x) = ln (x) / ln (b)
    return new Float64(Math.log(this.value) / Math.log(rhs.value));
  }
  
  @Override
  public Complex log(Complex rhs)
  {
    return toComplex().log(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Int rhs)  
  {
    return isEqual(rhs.toFloat64());
  }
  
  @Override // IEq
  public boolean isEqual(BigInt rhs)
  {
    return isEqual(rhs.toFloat64());
  }
  
  @Override // IEq
  public boolean isEqual(Ratio rhs)  
  {
    return isEqual(rhs.toFloat64());
  }
  
  @Override // IEq
  public boolean isEqual(Float64 rhs)
  {
    return this.value == rhs.value;
  }
  
  @Override // IEq
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }
  
  @Override // IOrd
  public int compareTo(Int rhs)
  {
    return compareTo(rhs.toFloat64());
  }
  
  @Override // IOrd
  public int compareTo(BigInt rhs)
  {
    return compareTo(rhs.toFloat64());
  }
  
  @Override // IOrd
  public int compareTo(Ratio rhs)
  {
    return compareTo(rhs.toFloat64());
  }
  
  @Override // IOrd
  public int compareTo(Float64 that)
  {
    return Double.compare(this.value, that.value);
  }
  
  @Override // IComplex
  public Float64 realPart()
  {
    return this;
  }
  
  @Override // IComplex
  public IReal imagPart()
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
  
  @Override // IComplex
  public boolean isFinite()
  {
    return Double.isFinite(value);
  }
  
  @Override // IComplex
  public boolean isNaN()
  {
    return Double.isNaN(value);
  }
  
  @Override // INum
  public boolean isInteger()
  {
    // A double is considered an integer according to the integer? predicate if
    // and only if it is finite, has 0 fractional part, and is not NaN.
    return Double.isFinite(value) && Math.floor(value) == value;
  }
  
  @Override // INum
  public boolean isReal()
  {
    return true; // The imaginary part is implicitly 0
  }
  
  @Override // INum
  public boolean isRational()
  {
    // If this double value is considered an integer, then it is also rational
    // with an implicit denominator of 1.
    // NOTE: Should all doubles be considered rational?
    //return isInteger();
    return Double.isFinite(value) && !Double.isNaN(value);
  }
  
  @Override // INum
  public boolean isComplex()
  {
    // Every real is a complex with 0 imaginary part
    return true;
  }
  
  @Override // INum
  public boolean isZero()
  {
    return value == 0.0;
  }
  
  @Override // IReal
  public boolean isPositive()
  {
    return value > 0.0;
  }
  
  @Override // IReal
  public boolean isNegative()
  {
    return value < 0.0;
  }
  
  @Override // INum
  public boolean isExact()
  {
    return false;
  }
  
  /*
  @Override // INum
  public IInt toExact()
  {
    try {
      var big = BigDecimal.valueOf(value).toBigIntegerExact();
      
      // as an optimization, attempt to convert the BigInteger to a long 
      try {
        var small = big.longValueExact();
        return Int.valueOf(small);
      }
      catch (ArithmeticException ex) {
        // the value cannot fit in a long, keep as is
        return new BigInt(big);
      }
    }
    catch (NumberFormatException ex) {
      // value is infinite or NaN
      throw new InvalidArgument("cannot convert infinite or NaN values to exact");
    }
    catch (ArithmeticException ex) {
      // fractional part was non-zero
      throw new InvalidArgument("cannot convert non-integer valued number to exact number");
    }
  }
  */
  
  @Override // INum
  public IRatio toExact()
  {
    return rationalize(this.value);
  }
  
  @Override // INum
  public INum toInexact()
  {
    return this;
  }
  
  @Override // IReal
  public IRatio rationalize()
  {
    return rationalize(this.value);
  }
    
  /**
   * Losslessly convert an IEEE 754 double precision floating point value
   * into a rational number (integer numerator and denominator).
   *
   * An IEEE 754 double-precision number is split into three parts:
   * a 1-bit sign S, an 11-bit biased exponent E, and a 52-bit fraction F.
   * Its exact mathematical value is:
   *
   * (-1)^S * (1 + (F / 2^52)) * 2^(E-1023)
   */
  public static Ratio rationalize(double val)
  {
    //if (isNaN() || !isFinite())
      //throw new InvalidArgument("cannot convert NaN or infinity to rational number");
    
    // Convert the double bits to a 64-bit long representation
    long bits = Double.doubleToLongBits(val);
    // Extract Sign Bit (1 bit at position 63)
    int sign = (int) (bits >> 63) & 1;
    // Extract Exponent (11 bits from positions 62 to 52)
    int biasedExp = (int) ((bits >> 52) & 0x7FF);
    // Compute true exponent (subtract implicit bias of 1023)
    int exp = biasedExp - 1023;
    // Extract Mantissa / Fraction (52 bits from positions 51 to 0)
    long frac = bits & 0xFFFFFFFFFFFFFL;
    // true fraction (for normalized values) = (1L << 52) + frac;
    
    System.err.println(String.format("doubleToLongBits: raw = 0x%x, sign = %d, exp = %d, frac = 0x%x", bits, sign, exp, frac));
    
    // Handle Infinities and NaN

    if (biasedExp == 0x7FF)
      throw new InvalidArgument("cannot convert NaN or infinity to rational number");
    
    // Handle zero
    // -0.0 and +0.0 are collapsed into 0/1 (which is non-negative).
    
    if (biasedExp == 0 && frac == 0)
      return Ratio.ZERO;
    
    // Handle subnormal values
    
    if (biasedExp == 0 && frac != 0) {
      System.err.println("subnormal");
      var num = BigInteger.valueOf(frac).multiply(BigInteger.ONE.shiftLeft(-1022));
      var den = BigInteger.ONE;
      if (sign > 0)
        num = num.negate();
      return new Ratio(num, den);
    }
    
    // Handle normalized values
    
    if (exp >= 52) {
      // num = sign * ((1L << 52) + frac) * (1L << (exp - 52));
      // den = 1;
      var num = BigInteger.ONE.shiftLeft(52).add(BigInteger.valueOf(frac))
                .multiply(BigInteger.ONE.shiftLeft(exp - 52));
      var den = BigInteger.ONE;
      if (sign > 0)
        num = num.negate();
      return new Ratio(num, den);
    }
    else { // (exp < 52)
      // num = sign * ((1L << 52) + frac);
      // den = (1L << (52 - exp));
      var num = BigInteger.ONE.shiftLeft(52).add(BigInteger.valueOf(frac));
      var den = BigInteger.ONE.shiftLeft(52 - exp);
      if (sign > 0)
        num = num.negate();
      return new Ratio(num, den);
    }
  }
  
  private final double value;
}
