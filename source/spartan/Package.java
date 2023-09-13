package spartan;

import spartan.data.Symbol;
import spartan.data.Datum;
import spartan.data.List;
import spartan.errors.InvalidArgument;
import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class Package
{
  public Package(Symbol name, Package parent)
  {
    this.name = name;
    this.parent = parent;
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
    bind(symbol, pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol)), () -> new MultipleDefinition(symbol));
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
    bind(alias, pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol)), () -> new MultipleDefinition(symbol));
  }
  
  /**
   * Import all symbols from another package into this package
   *
   * @param pkg The package to import from
   * @param symbol The symbol to import
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
    localPkgAliases.put(pkgName, pkg);
  }
  
  /**
   * Get a local package alias
   *
   * @param pkgName A package alias
   * @return The (optional) package aliased by pkgName
   */
  public Optional<Package> getPackageAlias(Symbol pkgName)
  {
    return Optional.ofNullable(localPkgAliases.get(pkgName));
  }
  
  /**
   * Bind a symbol to a value
   *
   * If the symbol is not present in this package, binds the given symbol to the given value.
   * Otherwise, throws a MultipleDefinition exception.
   *
   * @param name The symbol to bind
   * @param val The symbol's value
   * @throws MultipleDefinition If symbol is already present in this package
   */
  public void bind(Symbol name, Datum val, Supplier<MultipleDefinition> onError)
  {
    if (bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, val);
  }
  
  /**
   * Bind a symbol to a value
   *
   * If the symbol is already present in this package, its value is replaced.
   * 
   * @param name The symbol to bind
   * @param name The symbol's value
   */ 
  protected void bind(Symbol name, Datum val)
  {
    bindings.put(name, val);
  }
  
  /**
   * Set the value of a symbol
   *
   * If the symbol is present in this package, sets the value of the given symbol to the given value.
   * Otherwise, throws a UnboundSymbol exception.
   *
   * @param name The symbol to bind
   * @param val The symbol's value
   * @throws UnboundSymbol If symbol is not present in this package
   */
  public void store(Symbol name, Datum val, Supplier<UnboundSymbol> onError)
  {
    if (!bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, val);
  }
  
  /**
   * Lookup the value of a symbol
   * 
   * @param name The symbol to look up
   * @return The (optional) value of the symbol
   */
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name)).or(() -> parent.lookup(name));
  }
  
  public String toString()
  {
    return String.format("package %s %s", name.repr(),
      bindings.entrySet().stream()
              .map(e -> e.getKey().repr() + " => " + e.getValue().repr())
              .collect(Collectors.joining(", ", "{", "}")));
  }
  
  protected final Symbol name;
  protected final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  protected final Map<Symbol, Package> localPkgAliases = new IdentityHashMap<>();
  protected final Package parent;
}
