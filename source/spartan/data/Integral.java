package spartan.data;

public abstract sealed class Integral extends Numeric
permits Int, BigInt
{
  public abstract int intValue();
}
