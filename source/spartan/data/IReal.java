package spartan.data;

public sealed interface IReal extends INum permits Int, BigInt, Ratio, Real
{
  double toFloat64();
  IReal floor();
  IReal ceiling();
  IReal round();
}
