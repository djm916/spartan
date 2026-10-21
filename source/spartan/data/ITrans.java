package spartan.data;

/**
 * Extends the base set of numeric functions with the transcendental functions.
 *
 * In general, these operations are inherently approximate and are defined
 * only for inexact inputs and return inexact output.
 */
public sealed interface ITrans extends INum
permits Float64, Complex
{
  ITrans sin();
  ITrans cos();
  ITrans tan();
  ITrans asin();
  ITrans acos();
  ITrans atan();
  
  default ITrans exp(ITrans rhs)
  {
    return switch (rhs) {
      case Float64 r -> exp(r);
      case Complex c -> exp(c);
    };
  }
  
  ITrans exp(Float64 rhs);
  ITrans exp(Complex rhs);
  
  default ITrans log(ITrans rhs)
  {
    return switch (rhs) {
      case Float64 r -> log(r);
      case Complex c -> log(c);
    };
  }
  
  ITrans log(Float64 rhs);
  ITrans log(Complex rhs);
}
