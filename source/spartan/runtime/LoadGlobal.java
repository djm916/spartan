package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;

public record LoadGlobal(Symbol nsName, Symbol baseName, SourceInfo source, Inst next) implements Inst
{}
