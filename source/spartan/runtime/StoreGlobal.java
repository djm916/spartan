package spartan.runtime;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.util.Box;

public record StoreGlobal(Symbol symbol, Box<Datum> location, Inst next) implements Inst
{}
