package spartan;

import spartan.builtins.CoreNS;
import spartan.data.Symbol;
import spartan.data.QualifiedSymbol;
import spartan.data.Datum;
import spartan.data.Namespace;
import spartan.data.Macro;
import spartan.data.RecordDescriptor;
import spartan.errors.MultipleDefinition;
import spartan.errors.UnboundSymbol;
import spartan.errors.NoSuchNamespace;
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
  public static Namespace currentNS()
  {
    return currentNS;
  }
  
  /**
   * Set the current namespace
   *
   * @param ns the namespace
   */
  public static void currentNS(Namespace ns)
  {
    currentNS = ns;
  }
  
  /**
   * Set the current namespace, creating it if it doesn't exist
   *
   * @param nsName the namespace name
   */
  public static void enterNS(Symbol nsName)
  {
    currentNS(getOrCreateNS(nsName));
  }
  
  /**
   * Find a namespace
   *
   * @param nsName the namespace name to search for
   * @return the namespace found
   * @throws NoSuchNamespace if no such namespace exists
   */
  public static Namespace getNS(Symbol nsName)
  {
    var ns = namespaces.get(nsName);
    if (ns == null)
      throw new NoSuchNamespace(nsName);
    return ns;
  }
  
  /**
   * Creates and returns a new namespace if it doesn't exist, otherwise returns the
   * existing namespace.
   */
  public static Namespace getOrCreateNS(Symbol nsName)
  {
    return namespaces.computeIfAbsent(nsName, (_) -> new Namespace(nsName, CoreNS.INSTANCE));
  }
  
  /**
   * Add a namespace, overwriting any existing mapping to an existing namespace.
   */
  public static void addNS(Namespace ns)
  {
    namespaces.put(ns.name(), ns);
  }
  
  public static Namespace createNS(Symbol nsName)
  {
    //if (namespaces.containsKey(nsName))
      //throw new DuplicateNamespace(nsName);
    var ns = new Namespace(nsName, CoreNS.INSTANCE);
    namespaces.put(nsName, ns);
    return ns;
  }
  
  public static Datum lookup(Symbol s)
  {
    return (s instanceof QualifiedSymbol qs)
           ? getNS(canonicalName(Symbol.of(qs.nameSpace()))).lookup(Symbol.of(qs.baseName()))
           : currentNS().lookup(s.intern());
  }
  
  private static Symbol canonicalName(Symbol nsName)
  {
    return spartan.Runtime.currentNS().lookupAlias(nsName).map(ns -> ns.name()).orElse(nsName);
  }
  
  /** Resolve the given symbol in the global environment
   *
   * @param s the symbol to look up
   * @return the value bound to the symbol
   * @throws NoSuchNamespace if the symbol is qualified and the namespace does not exist
   * @throws UnboundSymbol if the symbol could not be resolved
   */
  public static Optional<Datum> tryLookup(Symbol s)
  {
    try {
      return Optional.of(lookup(s));
    }
    catch (UnboundSymbol | NoSuchNamespace err) {
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
    var coreNS = CoreNS.INSTANCE;
    addNS(coreNS);
    currentNS(coreNS);
    var bootFile = Config.HOME_DIR.resolve(Path.of("stdlib", "spartan", "core", "core.s"));
    Loader.load(bootFile);
    var userNS = new Namespace(Symbol.of("user"), coreNS);
    addNS(userNS);
    currentNS(userNS);
  }
  
  private Runtime() { }
  
  private static Namespace currentNS;
  private static final Map<Symbol, Namespace> namespaces = new IdentityHashMap<>();
  private static final Logger log = Logger.getLogger(Runtime.class.getName());
}
