package spartan.data;

/**
 * Extends the base numeric interface with a set of functions specific to real numbers.
 */
public sealed interface IReal extends INum
permits Int, BigInt, Ratio, Real
{
  double doubleValue();
  IReal floor();
  IReal ceiling();
  IReal round();
  boolean isPositive();
  boolean isNegative();
  IRatio rationalize();
  String formatDec(int precision);
}
