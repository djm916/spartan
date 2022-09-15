package spartan.parsing;

import spartan.data.Datum;

/** A pair of an expression and its corresponding map of source positions */
public record SourceDatum(Datum datum, PositionMap positionMap) {}
