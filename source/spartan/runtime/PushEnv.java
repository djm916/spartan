package spartan.runtime;

public record PushEnv(int numSlots, Inst next) implements Inst
{}
