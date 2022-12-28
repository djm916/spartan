package spartan;

import spartan.data.*;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

public final class MacroEnv
{
  private final Map<Symbol, Macro> macros = new HashMap<>();
  
  public void bind(Symbol name, Macro val)
  {
    macros.put(name, val);
  }
  
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(macros.get(name));
  }
}
 