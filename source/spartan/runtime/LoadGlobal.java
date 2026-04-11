package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;

public record LoadGlobal(Symbol moduleName, Symbol baseName, SourceInfo source, boolean publicOnly, Inst next) implements Inst
{}
