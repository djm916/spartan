package spartan.runtime;

import spartan.compiling.IPattern;

public record Match(IPattern pattern, Inst target, Inst next) implements Inst
{}
