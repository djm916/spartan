package spartan.data;

/**
 * Extends the base numeric interface with a set of functions specific to real values.
 */
public sealed interface IReal extends INum
permits Int, BigInt, Ratio, Real
{
  double toFloat64();
  IInt floor();
  IInt ceiling();
  IInt round();
}
