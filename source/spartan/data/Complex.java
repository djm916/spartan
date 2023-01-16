package spartan.data;

public final class Complex implements Datum, INum, IEq<Complex>
{
  public static final Complex I = new Complex(0.0, 1.0);
    
  public Complex(double x, double y)
  {
    this.real = x;
    this.imag = y;
  }
  
  public Complex(IReal x, IReal y)  
  {
    this(x.doubleValue(), y.doubleValue());
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
  
  @Override
  public Complex neg()
  {
    return new Complex(-real, -imag);
  }
  
  @Override
  public Real abs()
  {
    return new Real(Math.hypot(real, imag));
  }
  
  @Override // IEq
  public boolean isEqual(Complex that)
  {
    return this.real == that.real && this.imag == that.imag;
  }
  
  @Override
  public Complex add(Int rhs)
  {
    return add(rhs.toComplex());
  }
  
  @Override
  public Complex add(BigInt rhs)
  {
    return add(rhs.toComplex());
  }
  
  @Override
  public Complex add(Ratio rhs)
  {
    return add(rhs.toComplex());
  }
  
  @Override
  public Complex add(Real rhs)
  {
    return add(rhs.toComplex());
  }
  
  @Override
  public Complex add(Complex that)
  {
    return new Complex(this.real + that.real, this.imag + that.imag);
  }
  
  @Override
  public Complex sub(Int rhs)
  {
    return sub(rhs.toComplex());
  }
  
  @Override
  public Complex sub(BigInt rhs)
  {
    return sub(rhs.toComplex());
  }
  
  @Override
  public Complex sub(Ratio rhs)
  {
    return sub(rhs.toComplex());
  }
  
  @Override
  public Complex sub(Real rhs)
  {
    return sub(rhs.toComplex());
  }
  
  @Override
  public Complex sub(Complex that)
  { 
    return new Complex(this.real - that.real, this.imag - that.imag);
  }
  
  @Override
  public Complex mul(Int rhs)
  {
    return mul(rhs.toComplex());
  }
  
  @Override
  public Complex mul(BigInt rhs)
  {
    return mul(rhs.toComplex());
  }
  
  @Override
  public Complex mul(Ratio rhs)
  {
    return mul(rhs.toComplex());
  }
  
  @Override
  public Complex mul(Real rhs)
  {
    return mul(rhs.toComplex());
  }
  
  /* Complex number multiplication procedure.
     
     let x = a + b*i, y = c + d*i
     
     xy = (a + b*i)(c + d*i)
        = a*c + a*d*i + b*c*i + b*d*i^2
        = a*c + a*d*i + b*c*i - b*d
        = (a*c - b*d) + (a*d + b*c)*i
  */
  
  @Override
  public Complex mul(Complex rhs)
  {
    var a = this.real;
    var b = this.imag;
    var c = rhs.real;
    var d = rhs.imag;
    
    return new Complex(a * c - b * d, a * d + b * c);
  }
  
  @Override
  public Complex div(Int rhs)
  {
    return div(rhs.toComplex());
  }
  
  @Override
  public Complex div(BigInt rhs)
  {
    return div(rhs.toComplex());
  }
  
  @Override
  public Complex div(Ratio rhs)
  {
    return div(rhs.toComplex());
  }
  
  @Override
  public Complex div(Real rhs)
  {
    return div(rhs.toComplex());
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
  
  @Override
  public Complex div(Complex rhs)
  {
    var a = this.real;
    var b = this.imag;
    var c = rhs.real;
    var d = rhs.imag;
    
    double s = 1.0 / (c * c + d * d);
    return new Complex(s * (a * c + b * d), s * (b * c - a * d));
  }
  
  @Override
  public Complex sin()
  {
    return csin(real, imag);
  }
  
  @Override
  public Complex cos()
  {
    return ccos(real, imag);
  }
  
  @Override
  public Complex tan()
  {
    return ctan(real, imag);
  }
  
  @Override
  public Complex asin()
  {
    return casin(real, imag);
  }
  
  @Override
  public Complex acos()
  {
    return cacos(real, imag);
  }
  
  @Override
  public Complex atan()
  {
    return catan(real, imag);
  }
  
  @Override
  public Complex exp(Int rhs)
  {
    return exp(rhs.toComplex());
  }
  
  @Override
  public Complex exp(BigInt rhs)
  {
    return exp(rhs.toComplex());
  }
  
  @Override
  public Complex exp(Ratio rhs)
  {
    return exp(rhs.toComplex());
  }
  
  @Override
  public Complex exp(Real rhs)
  {
    return exp(rhs.toComplex());
  }
  
  @Override
  public Complex exp(Complex rhs)
  {
    return cexp(this.real, this.imag, rhs.real, rhs.imag);
  }
  
  @Override
  public Complex log(Int rhs)
  {
    return log(rhs.toComplex());
  }
  
  @Override
  public Complex log(BigInt rhs)
  {
    return log(rhs.toComplex());
  }
  
  @Override
  public Complex log(Ratio rhs)
  {
    return log(rhs.toComplex());
  }
  
  @Override
  public Complex log(Real rhs)
  {
    return log(rhs.toComplex());
  }
  
  @Override
  public Complex log(Complex rhs)
  {
    return clog(this.real, this.imag, rhs.real, rhs.imag);
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
