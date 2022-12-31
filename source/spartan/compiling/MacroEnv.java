package spartan.compiling;

import spartan.data.Symbol;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

class MacroEnv
{
  private static final Map<Symbol, Macro> bindings = new HashMap<>();
  
  private MacroEnv() {}
    
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public static void bind(Symbol name, Macro val)
  {
    bindings.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public static Optional<Macro> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name));
  }
}
