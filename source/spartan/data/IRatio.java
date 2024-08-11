package spartan.data;

/** Extends the base numeric interface with a set of functions specific to rational numbers. */
public sealed interface IRatio extends INum permits Int, BigInt, Ratio
{
  double doubleValue();
  IInt numerator();
  IInt denominator();
}
