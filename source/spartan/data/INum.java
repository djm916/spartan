package spartan.data;

/**
 * Base interface for all numeric types (integer, rational, real, and complex).
 *
 * Contains the common subset of mathematical functions which apply to all numeric types.
 */
public sealed interface INum extends Datum
permits IInt, IRatio, IReal, IComplex, ITrans, Int, BigInt, Ratio, Float64, Complex
{
  default INum add(INum rhs)
  {
    return switch (rhs) {
      case Int z -> add(z);
      case BigInt z -> add(z);
      case Ratio q -> add(q);
      case Float64 r -> add(r);
      case Complex c -> add(c);
    };
  }
  
  INum add(Int rhs);
  INum add(BigInt rhs);
  INum add(Ratio rhs);
  INum add(Float64 rhs);
  INum add(Complex rhs);
  
  default INum sub(INum rhs)
  {
    return switch (rhs) {
      case Int z -> sub(z);
      case BigInt z -> sub(z);
      case Ratio q -> sub(q);
      case Float64 r -> sub(r);
      case Complex c -> sub(c);
    };
  }
  
  INum sub(Int rhs);
  INum sub(BigInt rhs);
  INum sub(Ratio rhs);
  INum sub(Float64 rhs);
  INum sub(Complex rhs);
  
  default INum mul(INum rhs)
  {
    return switch (rhs) {
      case Int z -> mul(z);
      case BigInt z -> mul(z);
      case Ratio q -> mul(q);
      case Float64 r -> mul(r);
      case Complex c -> mul(c);
    };
  }
  
  INum mul(Int rhs);
  INum mul(BigInt rhs);
  INum mul(Ratio rhs);
  INum mul(Float64 rhs);
  INum mul(Complex rhs);
  
  default INum div(INum rhs)
  {
    return switch (rhs) {
      case Int z -> div(z);
      case BigInt z -> div(z);
      case Ratio q -> div(q);
      case Float64 r -> div(r);
      case Complex c -> div(c);
    };
  }
  
  INum div(Int rhs);
  INum div(BigInt rhs);
  INum div(Ratio rhs);
  INum div(Float64 rhs);
  INum div(Complex rhs);
  
  INum neg();
  INum abs();
    
  boolean isInteger();
  boolean isRational();
  boolean isReal();
  boolean isComplex();
  
  boolean isZero();
  boolean isFinite();
  //boolean isInfinite();
  boolean isNaN();
  boolean isExact();
  INum toExact();
  INum toInexact();
}
