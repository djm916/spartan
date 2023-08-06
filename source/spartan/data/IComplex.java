package spartan.data;

/**
 * Extends the base numeric interface with a set of functions specific to complex numbers.
 */
public sealed interface IComplex extends INum permits Int, BigInt, Ratio, Real, Complex
{
  IReal real();
  IReal imag();
  IReal angle();
  IReal magnitude();
}
