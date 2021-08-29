package spartan.data;

public class Complex extends Datum
{
  public Complex(double real, double imag)
  {
    this.real = real;
    this.imag = imag;
  }
  
  public Complex(Real x, Real y)  
  {
    this.real = x.value;
    this.imag = y.value;
  }
  
  public final Type type()
  {
    return Type.Complex;
  }
  
  public final String repr()
  {
    return String.format("(complex %s %s)",
                         Double.toString(real),
                         Double.toString(imag));
  }
  
  public final Complex neg()
  {
    return null;
  }
  
  public final Complex abs()
  {
    return null;
  }
  
  public final static Complex add(Complex x, Complex y)
  {
    return new Complex(x.real + y.real, x.imag + y.imag);
  }
  
  public final static Complex sub(Complex x, Complex y)
  {
    return new Complex(x.real - y.real, x.imag - y.imag);
  }
  
  public final static Complex mul(Complex x, Complex y)
  {
    return new Complex(x.real * y.real - x.imag * y.imag,
                       x.real * y.imag + x.imag * y.real);
  }
  
  public final static Complex div(Complex that)
  {
    return null;
  }
  
  public final static boolean eq(Complex that)
  {
    return false;
  }
  
  public static Complex log(Complex x, Complex y)
  {
    return clog(x.real, x.imag, y.real, y.imag);
  }
  
  public static Complex exp(Complex x, Complex y)
  {
    return cexp(x.real, x.imag, y.real, y.imag);
  }
  
  public static Complex sin(Complex x)
  {
    return csin(x.real, x.imag);
  }
  
  public static Complex cos(Complex x)
  {
    return ccos(x.real, x.imag);
  }
  
  public static Complex tan(Complex x)
  {
    return ctan(x.real, x.imag);
  }
  
  public static Complex asin(Complex x)
  {
    return casin(x.real, x.imag);
  }
  
  public static Complex acos(Complex x)
  {
    return cacos(x.real, x.imag);
  }
  
  public static Complex atan(Complex x)
  {
    return catan(x.real, x.imag);
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
