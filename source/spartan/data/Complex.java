package spartan.data;

public final class Complex extends Datum
{
  public static final Complex I = new Complex(0.0, 1.0);
  
  public Complex(double x, double y)
  {
    this.real = x;
    this.imag = y;
  }
  
  public Complex(Real x, Real y)  
  {
    this(x.value(), y.value());
  }
  
  public Type type()
  {
    return Type.Complex;
  }
  
  public String repr()
  {
    return String.format("%g%+gi", real, imag);
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
  
  public static boolean eq(Complex x, Complex y)
  {
    return x.real == y.real && x.imag == y.imag;
  }
  
  public static Complex add(Complex x, Complex y)
  {
    return new Complex(x.real + y.real, x.imag + y.imag);
  }
  
  public static Complex sub(Complex x, Complex y)
  {
    return new Complex(x.real - y.real, x.imag - y.imag);
  }
  
  public static Complex mul(Complex x, Complex y)
  {
    return new Complex(x.real * y.real - x.imag * y.imag,
                       x.real * y.imag + x.imag * y.real);
  }
  
  // x = a + b*i, y = c + d*i
  // conj(y) = c - d*i
  // y * conj(y) = (c + d*i) * (c - d*i) = c^2 + d^2
  // x / y = (x / y) * (conj(y) / conj(y))
  //       = (x * conj(y)) / (c^2 + d^2)
  //       = ((a + b*i) * (c - d*i)) / (c^2 + d^2)
  //       = ((a*c + b*d) + (b*c - a*d)*i) / (c^2 + d^2)
  //       = ((a*c + b*d) / (c^2 + d^2)) + ((b*c - a*d) / (c^2 + d^2))*i
  
  public static Complex div(Complex x, Complex y)
  {
    double scale = 1.0 / (y.real * y.real + y.imag * y.imag);
    return new Complex(scale * (x.real * y.real + x.imag * y.imag),
                       scale * (x.imag * y.real - x.real * y.imag));
  }
  
  public static Complex log(Complex x, Complex y)
  {
    return clog(x.real, x.imag, y.real, y.imag);
  }
  
  public static Complex exp(Complex x, Complex y)
  {
    return cexp(x.real, x.imag, y.real, y.imag);
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
