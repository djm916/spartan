package spartan.runtime;

public record BranchFalse(Inst next, Inst target) implements Inst
{}
