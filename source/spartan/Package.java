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
  /*
  public Package(Symbol name)
  {
    this.name = name;
  }
  */
  
  public Package(Symbol name, Package parent)
  {
    this.name = name;
    this.parent = parent;
  }
  
  public Symbol name()
  {
    return name;
  }
  
  public void doImport(Package pkg, Symbol symbol)
  {
    bind(symbol, pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol)));
  }
  
  public void doImport(Package pkg, Symbol symbol, Symbol alias)
  {
    bind(alias, pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol)));
  }
  
  public void doImport(Package p)
  {
    for (var s : p.bindings.entrySet()) {
      doImport(p, s.getKey());
    }
  }
  
  public void addPackageAlias(Symbol s, Package p)
  {
    packageAliases.put(s, p);
  }
  
  public Optional<Package> getPackageAlias(Symbol pkgName)
  {
    return Optional.ofNullable(packageAliases.get(pkgName));
  }
  
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  
  public void bind(Symbol name, Datum val)
  {
    bindings.put(name, val);
  }
  
  public void bind(Symbol name, Datum val, Supplier<MultipleDefinition> onError)
  {
    if (bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, val);
  }
  
  public void store(Symbol name, Datum val, Supplier<UnboundSymbol> onError)
  {
    if (!bindings.containsKey(name))
      throw onError.get();
    bindings.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name)).or(() -> parent == null ? Optional.empty() : parent.lookup(name));
  }
  
  public String toString()
  {
    return String.format("package %s %s", name.repr(),
      bindings.entrySet().stream()
              .map(e -> e.getKey().repr() + " => " + e.getValue().repr())
              .collect(Collectors.joining(", ", "{", "}")));
  }
  
  private final Symbol name;
  private final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  private final Map<Symbol, Package> packageAliases = new IdentityHashMap<>();
  private final Package parent;
}
