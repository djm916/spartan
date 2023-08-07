package spartan.data;

public sealed interface IRatio extends INum permits Int, BigInt, Ratio
{
  double doubleValue();
  IInt numerator();
  IInt denominator();
}
