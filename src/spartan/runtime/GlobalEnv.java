package spartan.runtime;

import spartan.data.Datum;
import spartan.data.Symbol;
import java.util.Map;
import java.util.IdentityHashMap;

class GlobalEnv
{
  private final Map<Symbol, Datum> globals = new IdentityHashMap<>();

  final void bind(Symbol s, Datum x)
  {
    globals.put(s, x);
  }
  
  final Datum lookup(Symbol s)
  {
    return globals.get(s);
  }
}
