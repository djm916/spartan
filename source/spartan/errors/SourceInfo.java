package spartan.errors;

import spartan.data.Datum;
import spartan.parsing.Position;

public record SourceInfo(Datum exp, Position position) {}
