package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;

public class NameSpace
{
  public NameSpace(Symbol name)
  {
    this.name = name;
  }
    
  public Symbol name()
  {
    return name;
  }
    
  public void importUnchecked(NameSpace ns)
  {
    this.bindings.putAll(ns.bindings);
  }
  
  public void importUnchecked(NameSpace ns, Symbol s)
  {
    this.bindings.put(s, ns.bindings.get(s));
  }
  
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public void bind(Symbol name, Datum val)
  {
    bindings.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name));
  }
  
  private final Symbol name;
  private final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
}
