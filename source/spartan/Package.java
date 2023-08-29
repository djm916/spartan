package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.data.List;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;

public class Package
{
  public Package(Symbol name)
  {
    this.name = name;
  }
    
  public Symbol name()
  {
    return name;
  }
    
  public void importUnchecked(Package pkg)
  {
    this.vars.putAll(pkg.vars);
  }
  
  public void importUnchecked(Package pkg, Symbol s, Symbol r)
  {
    this.vars.put(r, pkg.vars.get(s));
  }
  
  public void importChecked(Package pkg, List vars)
  {
    /*
    for (; !vars.isEmpty(); vars = vars.cdr()) {
      if (!(vars.car() instanceof Symbol s))
        throw new TypeMismatch();
      bind(s, pkg.lookup(s));
    }
    */
  }
  
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public void bind(Symbol name, Datum val)
  {
    vars.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(vars.get(name));
  }
  
  private final Symbol name;
  private final Map<Symbol, Datum> vars = new IdentityHashMap<>();
  private final Map<Symbol, Macro> macros = new HashMap<>();
}
