package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;

public class NameSpace
{
  public NameSpace(Symbol name, NameSpace parent)
  {
    this.name = name;
    this.parent = parent;
  }
    
  public Symbol name()
  {
    return name;
  }
  
  public NameSpace parent()
  {
    return parent;
  }
  
  public void importAll(NameSpace other)
  {
    this.bindings.putAll(other.bindings);
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
    var val = bindings.get(name);
    if (val == null)
      return (parent == null) ? Optional.empty() : parent.lookup(name);
    else
      return Optional.of(val);
  }
  
  private final NameSpace parent;
  private final Symbol name;
  private final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
}
