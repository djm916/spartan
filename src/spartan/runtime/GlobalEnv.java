package spartan.runtime;

import spartan.data.Value;
import spartan.data.Symbol;
import java.util.Map;
import java.util.IdentityHashMap;

public class GlobalEnv
{
  private final Map<Symbol, Value> globals = new IdentityHashMap<>();

  public void bind(Symbol s, Value v)
  {
    globals.put(s, v);
  }
  
  public Value lookup(Symbol s)
  {
    return globals.get(s);
  }
}
