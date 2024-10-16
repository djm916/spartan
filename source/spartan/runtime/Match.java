package spartan.runtime;

import spartan.compiling.IPattern;

public record Match(IPattern pattern, Inst left, Inst right) implements Inst
{}
