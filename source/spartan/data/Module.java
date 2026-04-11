package spartan.data;

import spartan.errors.UnboundSymbol;
import spartan.errors.MultipleDefinition;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;
import java.util.Set;

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
    //_import(parent);
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
    if (!spartan.Config.ALLOW_REDEFINITION && bindingMap.containsKey(symbol))
      throw new MultipleDefinition(symbol);
    bind(alias, module.lookupPublic(symbol));
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
  
  /**
   * Bind a variable. The symbol is not exported.
   *
   * @param name The symbol to bind
   * @param value The symbol's value
   * @throws MultipleDefinition If symbol is already bound in this module
   */
  public void bind(Symbol name, Datum value)
  {
    if (!spartan.Config.ALLOW_REDEFINITION && bindingMap.containsKey(name))
      throw new MultipleDefinition(name);
    bindingMap.put(name, value);
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
   * Update a variable
   *
   * @param name The symbol to update
   * @param value The symbol's new value
   * @throws UnboundSymbol If symbol is not bound in this module
   */
  public void update(Symbol name, Datum value)
  {
    update(name, value, false);
  }
  
  /**
   * Update a variable
   *
   * @param name The symbol to update
   * @param value The symbol's new value
   * @throws UnboundSymbol If symbol is not bound and public in this module
   */
  public void updatePublic(Symbol name, Datum value)
  {
    update(name, value, true);
  }
  
  public void update(Symbol name, Datum value, boolean publicOnly)
  {
    if (!bindingMap.containsKey(name) || (publicOnly && !exportSet.contains(name)))
      throw new UnboundSymbol(name);
    bindingMap.put(name, value);
  }

  /**
   * Lookup a variable
   * 
   * @param name The symbol to look up
   * @return The value of the symbol
   * @throws UnboundSymbol If symbol is not accessible in this module
   */
  public Datum lookup(Symbol name)
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
  public Datum lookupPublic(Symbol name)
  {
    return lookup(name, true);
  }

  public Datum lookup(Symbol name, boolean publicOnly)
  {
    var value = bindingMap.get(name);
    if (value != null && (!publicOnly || (publicOnly && exportSet.contains(name))))
      return value;
    if (parent != null)
      return parent.lookupPublic(name);
    throw new UnboundSymbol(name);
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
    return bindingMap.keySet();
  }
  
  protected final Symbol name;
  protected final Module parent;
  protected final Map<Symbol, Datum> bindingMap = new IdentityHashMap<>();
  protected final Map<Symbol, Symbol> aliasMap = new IdentityHashMap<>();
  protected final Set<Symbol> exportSet = java.util.Collections.newSetFromMap(new IdentityHashMap<>());
}
