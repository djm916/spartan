package spartan.data;

/**
 * Extends the base set of numeric functions with functions defined on integer,
 * rational, and real (but not complex) numbers.
 */
public sealed interface IReal extends INum
permits Int, BigInt, Ratio, Float64
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
