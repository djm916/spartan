package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;

public record StoreGlobal(Symbol packageName, Symbol baseName, SourceInfo source, Inst next) implements Inst
{}
