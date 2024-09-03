package spartan.runtime;

public record StoreLocal(int depth, int offset, Inst next) implements Inst
{}
