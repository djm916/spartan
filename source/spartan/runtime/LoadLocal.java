package spartan.runtime;

public record LoadLocal(int depth, int offset, Inst next) implements Inst
{}
