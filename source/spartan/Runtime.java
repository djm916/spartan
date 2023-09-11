package spartan;

import spartan.builtins.CorePackage;
import spartan.data.Symbol;
import spartan.data.Datum;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public final class Runtime
{
  public static Package currentPackage()
  {
    return currentPackage;
  }
  
  public static void currentPackage(Package pkg)
  {
    currentPackage = pkg;
  }
  
  public static void enterPackage(Symbol pkgName)
  {
    currentPackage = getOrCreatePackage(pkgName);
  }
  
  public static Optional<Package> getPackage(Symbol pkgName)
  {
    //return Optional.ofNullable(packages.get(pkgName));
    return currentPackage.getPackageAlias(pkgName)
           .or(() -> Optional.ofNullable(packages.get(pkgName)));
  }
  
  public static Package getOrCreatePackage(Symbol pkgName)
  {
    return packages.computeIfAbsent(pkgName, (name) -> new Package(name, CorePackage.getInstance()));
  }
  
  public static void addPackage(Package pkg)
  {
    packages.put(pkg.name(), pkg);
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
