package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;

public record StoreGlobal(Symbol moduleName, Symbol baseName, SourceInfo source, boolean publicOnly, Inst next) implements Inst
{}
