package spartan.data;

public final class Complex extends Numeric
{
  public static final Complex I = new Complex(0.0, 1.0);
  
  public Complex(double x, double y)
  {
    this.real = x;
    this.imag = y;
  }
  
  public Complex(Real x, Real y)  
  {
    this(x.value, y.value);
  }
  
  public Complex(String real, String imag)
  {
    this(Double.parseDouble(real), Double.parseDouble(imag));
  }
  
  public Type type()
  {
    return Type.COMPLEX;
  }
  
  public String repr()
  {
    return String.format("%g%+gi", real, imag);
  }
  
  public Real real()
  {
    return new Real(real);
  }
  
  public Real imag()
  {
    return new Real(imag);
  }
  
  public Real magnitude()
  {
    return abs();
  }
  
  public Real angle()
  {
    return new Real(Math.atan2(imag, real));
  }
  
  public List toRect()
  {
    return List.of(new Real(real), new Real(imag));
  }
  
  public List toPolar()
  {
    return List.of(new Real(Math.hypot(real, imag)),
                   new Real(Math.atan2(imag, real)));
  }
  
  public Complex neg()
  {
    return new Complex(-real, -imag);
  }
  
  public Real abs()
  {
    return new Real(Math.hypot(real, imag));
  }
  
  public boolean eq(Complex that)
  {
    return this.real == that.real && this.imag == that.imag;
  }
  
  public Complex add(Complex that)
  {
    return new Complex(this.real + that.real, this.imag + that.imag);
  }
  
  public Complex sub(Complex that)
  { 
    return new Complex(this.real - that.real, this.imag - that.imag);
  }
  
  /* Complex number multiplication procedure.
     
     let x = a + b*i, y = c + d*i
     
     xy = (a + b*i)(c + d*i)
        = a*c + a*d*i + b*c*i + b*d*i^2
        = a*c + a*d*i + b*c*i - b*d
        = (a*c - b*d) + (a*d + b*c)*i
  */
  
  public Complex mul(Complex that)
  {
    var a = this.real;
    var b = this.imag;
    var c = that.real;
    var d = that.imag;
    
    return new Complex(a * c - b * d, a * d + b * c);
  }
  
  /* Complex number division procedure.
  
     Multiplying by the conjugate of the divisor.
     
     let x = a + bi, y = c + di
     
     conj(y) = c - di
     y * conj(y) = (c + d*i) * (c - d*i) = c^2 + d^2
     x / y = (x / y) * (conj(y) / conj(y))
           = (x * conj(y)) / (c^2 + d^2)
           = ((a + b*i) * (c - d*i)) / (c^2 + d^2)
           = ((a*c + b*d) + (b*c - a*d)*i) / (c^2 + d^2)
           = ((a*c + b*d) / (c^2 + d^2)) + ((b*c - a*d) / (c^2 + d^2))*i
  */
  public Complex div(Complex that)
  {
    var a = this.real;
    var b = this.imag;
    var c = that.real;
    var d = that.imag;
    
    double s = 1.0 / (c * c + d * d);
    return new Complex(s * (a * c + b * d), s * (b * c - a * d));
  }
  
  public Complex log(Complex that)
  {
    return clog(this.real, this.imag, that.real, that.imag);
  }
  
  public Complex exp(Complex that)
  {
    return cexp(this.real, this.imag, that.real, that.imag);
  }
  
  public Complex sin()
  {
    return csin(real, imag);
  }
  
  public Complex cos()
  {
    return ccos(real, imag);
  }
  
  public Complex tan()
  {
    return ctan(real, imag);
  }
  
  public Complex asin()
  {
    return casin(real, imag);
  }
  
  public Complex acos()
  {
    return cacos(real, imag);
  }
  
  public Complex atan()
  {
    return catan(real, imag);
  }
  
  public Complex catan()
  {
    return catan(real, imag);
  }
  
  private static native Complex clog(double x1, double y1, double x2, double y2);
  private static native Complex cexp(double x1, double y1, double x2, double y2);
  private static native Complex csin(double x, double y);
  private static native Complex ccos(double x, double y);
  private static native Complex ctan(double x, double y);
  private static native Complex casin(double x, double y);
  private static native Complex cacos(double x, double y);
  private static native Complex catan(double x, double y);
  
  private final double real;
  private final double imag;
}
