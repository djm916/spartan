package spartan.runtime;

public record JumpFalse(Inst target, Inst next) implements Inst
{}
