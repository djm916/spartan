package spartan.runtime;

public record JumpNoArgs(Inst target, Inst next) implements Inst
{}
