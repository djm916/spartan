package spartan.runtime;

import spartan.data.Datum;
import spartan.data.Symbol;
import spartan.data.Nil;
import java.util.Map;
import java.util.IdentityHashMap;

public class GlobalEnv
{
  private final Map<Symbol, Datum> globals = new IdentityHashMap<>();

  public void bind(Symbol s, Datum x)
  {
    globals.put(s, x);
  }
  
  public Datum lookup(Symbol s)
  {
    return globals.getOrDefault(s, Nil.Instance);
  }
}
