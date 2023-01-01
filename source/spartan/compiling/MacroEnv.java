package spartan.compiling;

import spartan.data.Symbol;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

class MacroEnv
{
  private static final Map<Symbol, Procedure> bindings = new HashMap<>();
  
  private MacroEnv() {}
    
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public static void bind(Symbol name, Procedure val)
  {
    bindings.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public static Optional<Procedure> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name));
  }
  
  public static boolean contains(Symbol name)
  {
    return bindings.containsKey(name);
  }
}
