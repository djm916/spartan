package spartan.data;

import spartan.errors.InvalidArgument;
import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;
import java.util.Set;

public class Namespace implements Datum
{
  public Namespace(Symbol name, Namespace parent)
  {
    this.name = name;
    this.parent = parent;
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.NAMESPACE;
  }
  
  @Override // Datum
  public String repr()
  {
    return String.format("#<%s %s>", type().name(), name.repr());
  }
  
  public Symbol name()
  {
    return name;
  }
  
  /**
   * Import a symbol from another namespace into this namespace
   *
   * @param ns The namespace to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in namespace
   * @throws MultipleDefinition if symbol is already present in this namespace
   */
  public void doImport(Namespace ns, Symbol symbol)
  {
    doImport(ns, symbol, symbol);
  }
  
  /**
   * Import a symbol from another namespace into this namespace, using an alias
   *
   * @param ns The namespace to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in namespace
   * @throws MultipleDefinition if symbol is already present in this namespace
   */
  public void doImport(Namespace ns, Symbol symbol, Symbol alias)
  {
    if (!spartan.Config.ALLOW_REDEFINITION && isAccessible(symbol))
      throw new MultipleDefinition(symbol);
    bind(alias, ns.lookup(symbol));
  }
  
  /**
   * Import all symbols from another namespace into this namespace
   *
   * @param ns The namespace to import from
   * @throws UnboundSymbol if symbol is not present in ns
   * @throws MultipleDefinition if symbol is already present in this namespace
   */
  public void doImport(Namespace ns)
  {
    for (var symbol : ns.bindings.keySet()) {
      doImport(ns, symbol);
    }
  }
  
  /**
   * Add a local namespace alias into this namespace
   *
   * @param nsName The alias for the namespace
   * @param ns The namespace to create an alias for
   */
  public void addAlias(Symbol alias, Symbol nsName)
  {
    aliases.put(alias.name(), nsName.name());
  }
  
  /**
   * Get the canonical name for an aliased namespace
   *
   * @param alias A namespace alias
   * @return The canonical namespace name
   */
  public Optional<String> canonicalName(String alias)
  {
    return Optional.ofNullable(aliases.get(alias));
  }
  
  /**
   * Bind a symbol to a value
   *
   * If the symbol is not present in this namespace, binds the given symbol to the given value.
   * Otherwise, throws a MultipleDefinition exception.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws MultipleDefinition If symbol is already present in this namespace
   */
  public void bind(Symbol name, Datum value)
  {
    if (!spartan.Config.ALLOW_REDEFINITION && bindings.containsKey(name))
      throw new MultipleDefinition(name);
    bindings.put(name, value);
  }
  
  /**
   * Set the value of a symbol
   *
   * If the symbol is present in this namespace, sets the value of the given symbol to the given value.
   * Otherwise, throws a UnboundSymbol exception.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws UnboundSymbol If symbol is not present in this namespace
   */
  public void store(Symbol name, Datum value)
  {
    if (!bindings.containsKey(name))
      throw new UnboundSymbol(name);
    bindings.put(name, value);
  }
  
  /**
   * Lookup the value of a symbol
   * 
   * @param name The symbol to look up
   * @return The value of the symbol
   * @throws UnboundSymbol If symbol is not visible in this namespace
   */
  public Datum lookup(Symbol name)
  {
    var value = bindings.get(name);
    if (value != null)
      return value;
    if (parent != null)
      value = parent.lookup(name);
    if (value == null)
      throw new UnboundSymbol(name);
    return value;
  }
  
  /**
   * Return the set of all symbols present in this namespace
   */
  public Set<Symbol> symbols()
  {
    return bindings.keySet();
  }
  
  private boolean isAccessible(Symbol name)
  {
    return bindings.containsKey(name) || (parent != null && parent.bindings.containsKey(name));
  }
  
  protected final Symbol name;
  protected final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  protected final Map<String, String> aliases = new HashMap<>();
  protected final Namespace parent;
}
 