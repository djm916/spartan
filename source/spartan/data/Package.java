package spartan.data;

import spartan.errors.InvalidArgument;
import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.Set;

public class Package implements Datum
{
  public Package(Symbol name, Package parent)
  {
    this.name = name;
    this.parent = parent;
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PACKAGE_TYPE;
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
   * Import a symbol from another package into this package
   *
   * @param pkg The package to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in package
   * @throws MultipleDefinition if symbol is already present in this package
   */
  public void doImport(Package pkg, Symbol symbol)
  {
    doImport(pkg, symbol, symbol);
  }
  
  /**
   * Import a symbol from another package into this package, using an alias
   *
   * @param pkg The package to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in package
   * @throws MultipleDefinition if symbol is already present in this package
   */
  public void doImport(Package pkg, Symbol symbol, Symbol alias)
  {
    bind(alias, pkg.lookup(symbol));
  }
  
  /**
   * Import all symbols from another package into this package
   *
   * @param pkg The package to import from
   * @throws UnboundSymbol if symbol is not present in package
   * @throws MultipleDefinition if symbol is already present in this package
   */
  public void doImport(Package pkg)
  {
    for (var symbol : pkg.bindings.keySet()) {
      doImport(pkg, symbol);
    }
  }
  
  /**
   * Add a local package alias into this package
   *
   * @param pkgName The alias for the package
   * @param pkg The package to create an alias for
   */
  public void addPackageAlias(Symbol pkgName, Package pkg)
  {
    aliases.put(pkgName, pkg);
  }
  
  /**
   * Get a local package alias
   *
   * @param pkgName A package alias
   * @return The (optional) package aliased by pkgName
   */
  public Package getPackageAlias(Symbol pkgName)
  {
    // size check is an optimization to avoid a call to IdentityHashMap.get in common case of a package having no local aliases
    return aliases.size() == 0 ? null : aliases.get(pkgName);
  }
  
  /**
   * Bind a symbol to a value
   *
   * If the symbol is not present in this package, binds the given symbol to the given value.
   * Otherwise, throws a MultipleDefinition exception.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws MultipleDefinition If symbol is already present in this package
   */
  public void bind(Symbol name, Datum value)
  {
    if (bindings.containsKey(name))
      throw new MultipleDefinition(name);
    bindings.put(name, value);
  }
  
  /**
   * Set the value of a symbol
   *
   * If the symbol is present in this package, sets the value of the given symbol to the given value.
   * Otherwise, throws a UnboundSymbol exception.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws UnboundSymbol If symbol is not present in this package
   */
  public void store(Symbol name, Datum value)
  {
    if (!bindings.containsKey(name))
      throw new UnboundSymbol(this.name, name);
    bindings.put(name, value);
  }
  
  /**
   * Lookup the value of a symbol
   * 
   * @param name The symbol to look up
   * @return The value of the symbol
   * @throws UnboundSymbol If symbol is not visible in this package
   */
  public Datum lookup(Symbol name)
  {
    var value = bindings.get(name);
    if (value != null)
      return value;
    if (parent != null)
      value = parent.lookup(name);
    if (value == null)
      throw new UnboundSymbol(this.name, name);
    return value;
  }
  
  /**
   * Return the set of all symbols present in this package
   */
  public Set<Symbol> symbols()
  {
    return bindings.keySet();
  }
  
  protected final Symbol name;
  protected final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  protected final Map<Symbol, Package> aliases = new IdentityHashMap<>();
  protected final Package parent;
}
