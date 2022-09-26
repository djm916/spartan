package spartan.data;

public abstract sealed class Numeric extends Datum
permits Integral, Ratio, Real, Complex
{}
