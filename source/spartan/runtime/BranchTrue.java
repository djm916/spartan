package spartan.runtime;

public record BranchTrue(Inst target, Inst next) implements Inst
{}
