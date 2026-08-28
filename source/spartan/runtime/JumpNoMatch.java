package spartan.runtime;

import spartan.compiling.IPattern;

public record JumpNoMatch(IPattern pattern, Inst target, Inst next) implements Inst
{}
