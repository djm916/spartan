package spartan.data;

import spartan.errors.InvalidArgument;
import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
//import spartan.util.Either;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.Set;

public class Package implements Datum
{
  public static final Package NONE = new Package(null, null) {
    public Optional<Datum> lookup(Symbol name)
    {
      return Optional.empty();
    }
  };
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PACKAGE_TYPE;
  }
  
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
    bind(alias, pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol)), () -> new MultipleDefinition(alias));
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
  public Optional<Package> getPackageAlias(Symbol pkgName)
  {
    return Optional.ofNullable(aliases.get(pkgName));
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
  public void bind(Symbol name, Datum value, Supplier<MultipleDefinition> onError)
  {
    if (bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, value);
  }
  
  public void bind(Symbol name, Datum value)
  {    
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
  public void store(Symbol name, Datum value, Supplier<UnboundSymbol> onError)
  {
    if (!bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, value);
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
  
  public Set<Symbol> symbols()
  {
    return bindings.keySet();
  }
  
  protected final Symbol name;
  protected final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  protected final Map<Symbol, Package> aliases = new IdentityHashMap<>();
  protected final Package parent;
}
