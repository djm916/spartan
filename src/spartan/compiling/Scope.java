package spartan.compiling;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.data.Symbol;

public class Scope
{
  private final Scope parent;
  private final Map<Symbol, Integer> bindings;
  
  public Scope(Scope parent)
  {
    this.parent = parent;
    this.bindings = new IdentityHashMap<>();
  }
  
  public DeBruijnIndex lookup(Symbol s)
  {
    return lookup(s, 0);
  }
  
  public boolean bind(Symbol s)
  {
    if (bindings.containsKey(s))
      return false;
    bindings.put(s, bindings.size());
    return true;
  }
  
  private DeBruijnIndex lookup(Symbol s, int depth)
  {
    if (bindings.containsKey(s))
      return new DeBruijnIndex(depth, bindings.get(s));
    else if (parent != null)
      return parent.lookup(s, depth + 1);
    else
      return null;
  }
}
