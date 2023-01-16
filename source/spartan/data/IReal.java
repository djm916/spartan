package spartan.data;

public sealed interface IReal extends INum permits Int, BigInt, Ratio, Real
{
  double doubleValue();
  IReal floor();
  IReal ceiling();
  IReal round();
}
