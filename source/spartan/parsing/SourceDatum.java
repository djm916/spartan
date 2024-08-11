package spartan.parsing;

import spartan.data.Datum;

/**
 * A {@code Datum} and its corresponding {@code PositionMap}.
 *
 * @param datum an s-expression
 * @param positionMap source code position data
 */
public record SourceDatum(Datum datum, PositionMap positionMap) {}
