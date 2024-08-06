package spartan;

import spartan.builtins.CorePackage;
import spartan.data.Symbol;
import spartan.data.QualifiedSymbol;
import spartan.data.Datum;
import spartan.data.Package;
import spartan.data.Macro;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.logging.Logger;
import java.nio.file.Path;

public final class Runtime
{
  /**
   * Returns the current package
   */
  public static Package currentPackage()
  {
    return (Package) CorePackage.INSTANCE.lookup(Symbol.of("*package*")).get();
    //return currentPackage;
  }
  
  /**
   * Set the current package
   */
  public static void currentPackage(Package pkg)
  {
    CorePackage.INSTANCE.bind(Symbol.of("*package*"), pkg);
    //currentPackage = pkg;
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
  public static Optional<Package> getPackage(Symbol pkgName)
  {
    //return Optional.ofNullable(packages.get(pkgName));
    return currentPackage().getPackageAlias(pkgName)
           .or(() -> Optional.ofNullable(packages.get(pkgName)));
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
  
  public static Optional<Macro> lookupMacro(Symbol s)
  {
    return lookup(s)
           .filter(datum -> datum instanceof Macro)
           .map(macro -> (Macro)macro);
    /*
    if (s instanceof QualifiedSymbol qs)
      return getPackage(Symbol.of(qs.packageName()))
             .flatMap(pkg -> pkg.lookup(Symbol.of(qs.baseName())));
             .filter(datum -> datum instanceof Macro)
             .map(macro -> (Macro)macro);
    else
      return currentPackage()
             .lookup(s.intern())
             .filter(datum -> datum instanceof Macro)
             .map(macro -> (Macro)macro);
     */
  }
  
  public static Optional<Datum> lookup(Symbol s)
  {
    if (s instanceof QualifiedSymbol qs)
      return getPackage(Symbol.of(qs.packageName()))
             .flatMap(pkg -> pkg.lookup(Symbol.of(qs.baseName())));
    else
      return currentPackage().lookup(s.intern());
  }
  
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
  
  //private static Package currentPackage = CorePackage.INSTANCE;
  private static final Map<Symbol, Package> packages = new IdentityHashMap<>();
  private static final Logger log = Logger.getLogger(Runtime.class.getName());
}
