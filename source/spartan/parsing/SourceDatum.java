package spartan.parsing;

import spartan.data.Datum;

/**
  A pair of an expression and its corresponding source map
*/
public record SourceDatum(Datum datum, PositionMap positionMap) {}
