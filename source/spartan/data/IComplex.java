package spartan.data;

public sealed interface IComplex extends INum permits Int, BigInt, Ratio, Real, Complex
{
  IReal realPart();
  IReal imagPart();
  IReal angle();
  IReal magnitude();
}
