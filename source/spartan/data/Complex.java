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
  
  // Computes the base x logarithm of y
  
  public static Complex log(Complex x, Complex y)
  {
    return log(x.real, x.imag, y.real, y.imag);
  }
  
  public static native Complex log(double x1, double y1, double x2, double y2);
  
  // Computes the base x exponent of y
  
  public static Complex exp(Complex x, Complex y)
  {
    return exp(x.real, x.imag, y.real, y.imag);
  }
  
  public static native Complex exp(double x1, double y1, double x2, double y2);
  
  private final double real;
  private final double imag;
}
