package spartan.runtime;

public record JumpTrue(Inst target, Inst next) implements Inst
{}
