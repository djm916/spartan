package spartan.parsing;

import spartan.data.Datum;

/**
 * A {@code Datum} and its corresponding {@code PositionMap}.
 */
public record SourceDatum(Datum datum, PositionMap positionMap) {}
