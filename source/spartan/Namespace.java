package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.data.List;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public class Namespace
{
  public Namespace(Symbol name)
  {
    this.name = name;
  }
    
  public Symbol name()
  {
    return name;
  }
  
  public void importUnchecked(Namespace ns)
  {
    this.bindings.putAll(ns.bindings);
  }
  
  public void importUnchecked(Namespace ns, Symbol s, Symbol r)
  {
    this.bindings.put(r, ns.bindings.get(s));
  }
  
  public void importChecked(Namespace ns, List bindings)
  {
    
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
