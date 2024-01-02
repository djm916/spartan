package spartan;

import spartan.builtins.CorePackage;
import spartan.data.Symbol;
import spartan.data.QualifiedSymbol;
import spartan.data.Datum;
import spartan.data.Package;
import spartan.compiling.Macro;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

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
    currentPackage = getOrCreatePackage(pkgName);
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
    return currentPackage.getPackageAlias(pkgName)
           .or(() -> Optional.ofNullable(packages.get(pkgName)));
  }
  
  /**
   * Creates and returns a new package if it doesn't exist, otherwise returns the
   * existing package.
   */
  public static Package getOrCreatePackage(Symbol pkgName)
  {
    return packages.computeIfAbsent(pkgName, (name) -> new Package(name, CorePackage.getInstance()));
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
    if (s instanceof QualifiedSymbol qs)
      return getPackage(Symbol.of(qs.packageName()))
             .flatMap(pkg -> pkg.lookup(Symbol.of(qs.baseName())))
             .flatMap(value -> value.right());
    else
      return currentPackage()
             .lookup(s.intern())
             .flatMap(value -> value.right());
  }
  
  public static Optional<Datum> lookup(Symbol s)
  {
    if (s instanceof QualifiedSymbol qs)
      return getPackage(Symbol.of(qs.packageName()))
             .flatMap(pkg -> pkg.lookup(Symbol.of(qs.baseName())))
             .flatMap(value -> value.left());
    else
      return currentPackage()
             .lookup(s.intern())
             .flatMap(value -> value.left());
  }
  
  public static void boot()
  {
    var corePackage = CorePackage.getInstance();
    addPackage(corePackage);
    currentPackage(corePackage);
    Loader.load(Config.BUILTINS_FILE_PATH);
    var userPackage = new Package(Symbol.of("user"), corePackage);
    addPackage(userPackage);
    currentPackage(userPackage);
  }
  
  private Runtime() { }
  
  private static final Map<Symbol, Package> packages = new IdentityHashMap<>();
  private static Package currentPackage;
}
