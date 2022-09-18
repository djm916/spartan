package spartan.data;

public abstract sealed class Numeric extends Datum
permits Int, BigInt, Ratio, Real, Complex
{}
