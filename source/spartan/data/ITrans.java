package spartan.data;

/**
 * Extends the set of base numeric functions with the transcendental functions.
 */
public sealed interface ITrans extends INum
permits Real, Complex
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
      case Real r -> exp(r);
      case Complex c -> exp(c);
    };
  }
  
  ITrans exp(Real rhs);
  ITrans exp(Complex rhs);
  
  default ITrans log(ITrans rhs)
  {
    return switch (rhs) {
      case Real r -> log(r);
      case Complex c -> log(c);
    };
  }
  
  ITrans log(Real rhs);
  ITrans log(Complex rhs);
}
