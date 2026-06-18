package spartan.runtime;

public record BranchTrue(Inst next, Inst target) implements Inst
{}
