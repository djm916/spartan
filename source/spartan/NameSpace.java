package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.compiling.Macro;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;
import spartan.errors.UnboundVariable;

public class NameSpace
{
  public NameSpace(String name)
  {
    this.name = name.intern();
  }
  
  public String name()
  {
    return name;
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
    return Optional.ofNullable(bindings.get(name));
  }
  
  private final String name;
  private final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
}
