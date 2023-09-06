package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.data.Macro;
import spartan.data.List;
import spartan.errors.InvalidArgument;
import spartan.errors.UnboundVariable;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public class Namespace
{
  public Namespace(Symbol name)
  {
    this.name = name;
  }
  
  public Namespace(Symbol name, Namespace imported)
  {
    this.name = name;
    importAllFrom(imported);
  }
  
  public Symbol name()
  {
    return name;
  }
  
  public void importChecked(Namespace ns, Symbol s)
  {
    bind(s, ns.lookup(s).orElseThrow(() -> new UnboundVariable(ns.name(), s)));
  }
  
  public void importFrom(Namespace ns, List args)
  {
    if (args.isEmpty())
      importAllFrom(ns);
    else {
      for (; !args.isEmpty(); args = args.cdr()) {
        if (!(args.car() instanceof Symbol s))
          throw new InvalidArgument();
        importChecked(ns, s);
      }
    }
  }
  
  public void importAllFrom(Namespace ns)
  {
    for (var s : ns.bindings.entrySet()) {
      importChecked(ns, s.getKey());
    }
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
