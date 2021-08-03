package spartan.runtime;

import spartan.data.Datum;
import spartan.data.Symbol;
import java.util.Map;
import java.util.IdentityHashMap;

public class GlobalEnv
{
  private final Map<Symbol, Datum> globals = new IdentityHashMap<>();

  public final void bind(Symbol s, Datum x)
  {
    globals.put(s, x);
  }
  
  public final Datum lookup(Symbol s)
  {
    return globals.get(s);
  }
}
