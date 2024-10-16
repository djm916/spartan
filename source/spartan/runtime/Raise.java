package spartan.runtime;

import spartan.errors.Error;

public record Raise(Error err, Inst next) implements Inst
{}
