package spartan.data;

import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
import spartan.util.Box;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Optional;

public class Module implements Datum
{
  public Module(Symbol name)
  {
    this(name, null);
  }
  
  public Module(Symbol name, Module parent)
  {
    this.name = name;
    this.parent = parent;
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.MODULE;
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
   * Import a symbol exported from another module into this module
   *
   * @param module The module to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in module
   * @throws MultipleDefinition if symbol is already bound in this module
   */
  public void _import(Module module, Symbol symbol)
  {
    _import(module, symbol, symbol);
  }
  
  /**
   * Import a symbol exported from another module into this module, renaming it
   *
   * @param module The module to import from
   * @param symbol The symbol to import
   * @throws UnboundSymbol if symbol is not present in module
   * @throws MultipleDefinition if symbol is already bound in this module
   */
  public void _import(Module module, Symbol symbol, Symbol alias)
  {
    var value = module.lookupPublic(symbol)
                .orElseThrow(() -> new UnboundSymbol(symbol))
                .get();
    bind(alias, value);
  }
  
  /**
   * Import all exported symbols from another module into this module
   *
   * @param module The module to import from
   * @throws UnboundSymbol if any symbol is not present in module
   * @throws MultipleDefinition if any symbol is already bound in this module
   */
  public void _import(Module module)
  {
    for (var symbol : module.exportSet) {
      _import(module, symbol);
    }
  }
  
  /**
   * Add a symbol to the set of symbols exported by this module
   *
   * @param symbol The symbol to export
   */
  public void export(Symbol name)
  {
    exportSet.add(name);
  }
  
  /**
   * Create an alias (alternate name) for a module, available only in this module
   *
   * @param alias The module's alias
   * @param moduleName The module to create an alias for
   */
  public void addAlias(Symbol alias, Symbol moduleName)
  {
    aliasMap.put(alias, moduleName);
  }
  
  /**
   * Lookup the canonical name of a module by its alias
   *
   * @param alias The module's alias
   * @return The canonical name of the module
   */
  public Optional<Symbol> lookupAlias(Symbol alias)
  {
    return Optional.ofNullable(aliasMap.get(alias));
  }
    
  public Box<Datum> bind(Symbol name)
  {
    return bindings.computeIfAbsent(name, (_) -> new Box<Datum>());
  }
  
  /**
   * Bind a variable. The symbol is not exported.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws MultipleDefinition If symbol is already bound in this module
   */
  public void bind(Symbol name, Datum value)
  {
    bind(name).set(value);
  }
  
  /**
   * Bind and export a variable.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws MultipleDefinition If symbol is already bound in this module
   */
  public void bindPublic(Symbol name, Datum value)
  {
    bind(name, value);
    export(name);
  }
  
  /**
   * Lookup a variable
   * 
   * @param name The symbol to look up
   * @return The value of the symbol
   * @throws UnboundSymbol If symbol is not accessible in this module
   */
  public Optional<Box<Datum>> lookup(Symbol name)
  {
    return lookup(name, false);
  }
  
  /**
   * Lookup a variable
   * 
   * @param name The symbol to look up
   * @return The value of the symbol
   * @throws UnboundSymbol If symbol is not publicly accessible in this module
   */
  public Optional<Box<Datum>> lookupPublic(Symbol name)
  {
    return lookup(name, true);
  }

  public Optional<Box<Datum>> lookup(Symbol name, boolean publicOnly)
  {
    var loc = bindings.get(name);
    if (loc != null && (!publicOnly || (publicOnly && exportSet.contains(name))))
      return Optional.of(loc);
    if (parent != null)
      return parent.lookupPublic(name);
    return Optional.empty();
  }
  
  /**
   * Return the set of all symbols publically accessible in this module
   */
  public Set<Symbol> symbolsPublic()
  {
    return exportSet;
  }
  
  /**
   * Return the set of all symbols bound in this module
   */
  public Set<Symbol> symbols()
  {
    return bindings.keySet();
  }
  
  protected final Symbol name;
  protected final Module parent;
  protected final Map<Symbol, Box<Datum>> bindings = new HashMap<>();
  protected final Map<Symbol, Symbol> aliasMap = new HashMap<>();
  protected final Set<Symbol> exportSet = new HashSet<>();
}
