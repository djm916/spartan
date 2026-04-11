package spartan;

import spartan.builtins.BaseModule;
import spartan.data.Symbol;
import spartan.data.QualifiedSymbol;
import spartan.data.Datum;
import spartan.data.Module;
import spartan.data.Macro;
import spartan.data.RecordDescriptor;
import spartan.errors.MultipleDefinition;
import spartan.errors.UnboundSymbol;
import spartan.errors.ModuleDoesNotExist;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.logging.Logger;
import java.nio.file.Path;

/** Global program execution state */
public final class Runtime
{
  /**
   * Returns the current namespace
   *
   * @return the current namespace
   */
  public static Module currentModule()
  {
    return currentModule;
  }
  
  /**
   * Set the current namespace
   *
   * @param ns the namespace
   */
  public static void currentModule(Module module)
  {
    currentModule = module;
  }
  
  /**
   * Set the current namespace, creating it if it doesn't exist
   *
   * @param moduleName the namespace name
   */
  public static void enterModule(Symbol moduleName)
  {
    currentModule(getOrCreateModule(moduleName));
  }
  
  /**
   * Find a namespace
   *
   * @param moduleName the namespace name to search for
   * @return the namespace found
   * @throws ModuleDoesNotExist if no such namespace exists
   */
  public static Module getModule(Symbol moduleName)
  {
    var module = modules.get(moduleName);
    if (module == null)
      throw new ModuleDoesNotExist(moduleName);
    return module;
  }
  
  /**
   * Creates and returns a new namespace if it doesn't exist, otherwise returns the
   * existing namespace.
   */
  public static Module getOrCreateModule(Symbol moduleName)
  {
    return modules.computeIfAbsent(moduleName, (_) -> new Module(moduleName, BaseModule.INSTANCE));
  }
  
  /**
   * Add a namespace, overwriting any existing mapping to an existing namespace.
   */
  public static void addModule(Module module)
  {
    modules.put(module.name(), module);
  }
  
  public static Module createModule(Symbol moduleName)
  {
    var module = new Module(moduleName, BaseModule.INSTANCE);
    modules.put(moduleName, module);
    return module;
  }
  
  public static Datum lookup(Symbol s)
  {
    return (s instanceof QualifiedSymbol qs)
           ? getModule(canonicalName(Symbol.of(qs.moduleName()))).lookup(Symbol.of(qs.baseName()))
           : currentModule().lookup(s.intern());
  }
  
  private static Symbol canonicalName(Symbol moduleName)
  {
    return currentModule().lookupAlias(moduleName).orElse(moduleName);
  }
  
  /** Resolve the given symbol in the global environment
   *
   * @param s the symbol to look up
   * @return the value bound to the symbol
   * @throws ModuleDoesNotExist if the symbol is qualified and the namespace does not exist
   * @throws UnboundSymbol if the symbol could not be resolved
   */
  public static Optional<Datum> tryLookup(Symbol s)
  {
    try {
      return Optional.of(lookup(s));
    }
    catch (UnboundSymbol | ModuleDoesNotExist err) {
      return Optional.empty();
    }
  }
  
  public static Optional<Macro> lookupMacro(Symbol s)
  {
    return tryLookup(s).filter(Macro.class::isInstance).map(Macro.class::cast);
  }
  
  public static Optional<RecordDescriptor> lookupRTD(Symbol s)
  {
    return tryLookup(s).filter(RecordDescriptor.class::isInstance).map(RecordDescriptor.class::cast);
  }
  
  /** Bootstrap the initial system state and global environment.
   *
   *  <ul>
   *    <li>Add the "spartan.core" namespace</li>
   *    <li>Load the "builtins.s" file</li>
   *    <li>Set the current namespace to the "user" namespace</li>
   *  </ul>
   */
  public static void boot()
  {
    if (Config.LOG_DEBUG)
      log.info(() -> String.format("initializing runtime environment"));
    var baseModule = BaseModule.INSTANCE;
    addModule(baseModule);
    currentModule(baseModule);
    var bootFile = Config.HOME_DIR.resolve(Path.of("stdlib", "spartan", "base", "base.s"));
    Loader.load(bootFile);
    // NOTE: Ensure all bindings exported by the spartan.base module are actually defined,
    // or an "unbound variable" error with be thrown (with no source location) when
    // attempting to import into the user module here.
    var userModule = new Module(Symbol.of("user"), baseModule);
    addModule(userModule);
    currentModule(userModule);
  }
  
  private Runtime() { }
  
  private static Module currentModule;
  private static final Map<Symbol, Module> modules = new IdentityHashMap<>();
  private static final Logger log = Logger.getLogger(Runtime.class.getName());
}
