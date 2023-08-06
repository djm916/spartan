package spartan.data;

public sealed interface IRatio extends INum permits Int, BigInt, Ratio
{
  double toFloat64();
  IInt numerator();
  IInt denominator();
}
