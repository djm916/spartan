package spartan.runtime;

import spartan.data.Datum;

public record LoadConst(Datum value, Inst next) implements Inst
{}
