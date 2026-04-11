package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;

public record BindGlobal(Symbol moduleName, Symbol baseName, SourceInfo source, Inst next) implements Inst
{}
