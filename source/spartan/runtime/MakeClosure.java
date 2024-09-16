package spartan.runtime;

import spartan.compiling.Procedure;

public record MakeClosure(Procedure proc, Inst next) implements Inst
{}
