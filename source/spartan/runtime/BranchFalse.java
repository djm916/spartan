package spartan.runtime;

public record BranchFalse(Inst target, Inst next) implements Inst
{}
