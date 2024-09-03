package spartan.runtime;

import spartan.parsing.Position;

public record PushFrame(Inst returnTo, Position position, Inst next) implements Inst
{}
