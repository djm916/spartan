package spartan;

import spartan.builtins.CorePackage;
import spartan.data.Symbol;
import spartan.data.QualifiedSymbol;
import spartan.data.Datum;
import spartan.data.Package;
import spartan.data.Macro;
import spartan.errors.MultipleDefinition;
import spartan.errors.UnboundSymbol;
import spartan.errors.NoSuchPackage;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.logging.Logger;
import java.nio.file.Path;

/** Global program execution state */
public final class Runtime
{
  /**
   * Returns the current package
   */
  public static Package currentPackage()
  {
    return currentPackage;
  }
  
  /**
   * Set the current package
   */
  public static void currentPackage(Package pkg)
  {
    currentPackage = pkg;
  }
  
  /**
   * Creates the package if it doesn't exist, and sets it as the current package
   */
  public static void enterPackage(Symbol pkgName)
  {
    currentPackage(getOrCreatePackage(pkgName));
  }
  
  /**
   * Find a package
   *
   * First searches for a local package alias of the given name, then for a
   * global package with the given name.
   */
  public static Package getPackage(Symbol pkgName)
  {
    var pkg = currentPackage().getPackageAlias(pkgName);
    if (pkg != null)
      return pkg;
    pkg = packages.get(pkgName);
    if (pkg == null)
      throw new NoSuchPackage(pkgName);
    return pkg;
  }
  
  /**
   * Creates and returns a new package if it doesn't exist, otherwise returns the
   * existing package.
   */
  public static Package getOrCreatePackage(Symbol pkgName)
  {
    return packages.computeIfAbsent(pkgName, (name) -> new Package(name, CorePackage.INSTANCE));
  }
  
  /**
   * Add a package, overwriting any existing mapping to an existing package.
   */
  public static void addPackage(Package pkg)
  {
    packages.put(pkg.name(), pkg);
  }
  
  /** Resolve the given symbol in the global environment
   *
   * @param s the symbol to look up
   * @return the value bound to the symbol
   * @throws NoSuchPackage if the symbol is qualified and the package does not exist
   * @throws UnboundSymbol if the symbol could not be resolved
   */
  public static Datum lookup(Symbol s)
  {
    if (s instanceof QualifiedSymbol qs)
      return getPackage(Symbol.of(qs.packageName())).lookup(Symbol.of(qs.baseName()));
    else
      return currentPackage().lookup(s.intern());
  }
  
  public static Optional<Macro> lookupMacro(Symbol s)
  {
    try {
      var value = lookup(s);
      return (value instanceof Macro macro) ? Optional.of(macro) : Optional.empty();
    }
    catch (UnboundSymbol | NoSuchPackage err) {
      return Optional.empty();
    }
  }
  
  /** Bootstrap the initial system state and global environment.
   *
   *  <ul>
   *    <li>Add the "spartan.core" package</li>
   *    <li>Load the "builtins.s" file</li>
   *    <li>Set the current package to the "user" package</li>
   *  </ul>
   */
  public static void boot()
  {
    if (Config.LOG_DEBUG)
      log.info(() -> String.format("initializing runtime environment"));
    var corePackage = CorePackage.INSTANCE;
    addPackage(corePackage);
    currentPackage(corePackage);
    Loader.load(Config.HOME_DIR.resolve(Path.of("stdlib", "builtins.s")));
    var userPackage = new Package(Symbol.of("user"), corePackage);
    addPackage(userPackage);
    currentPackage(userPackage);
  }
  
  private Runtime() { }
  
  private static Package currentPackage;
  private static final Map<Symbol, Package> packages = new IdentityHashMap<>();
  private static final Logger log = Logger.getLogger(Runtime.class.getName());
}
