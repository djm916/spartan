package spartan.compiling;

import spartan.data.Symbol;
import spartan.data.Callable;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

public final class MacroEnv
{
  private final Map<String, Callable> macros = new HashMap<>();
  private static final MacroEnv instance = new MacroEnv();
  
  private MacroEnv() {}
  
  public static MacroEnv instance()
  {
    return instance;
  }
  
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public void bind(Symbol name, Callable val)
  {
    macros.put(name.str(), val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public Optional<Callable> lookup(Symbol name)
  {
    return Optional.ofNullable(macros.get(name.str()));
  }
}
