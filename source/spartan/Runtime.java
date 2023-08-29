package spartan;

import spartan.builtins.CorePkg;
import spartan.builtins.ListPkg;
import spartan.builtins.VectorPkg;
import spartan.builtins.StringPkg;
import spartan.builtins.PortPkg;
import spartan.data.Symbol;
import spartan.compiling.Macro;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
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
  
  public static void enterPackage(Symbol pkg)
  {
    currentPackage = getOrCreatePackage(pkg);
  }
  
  public static Optional<Package> getPackage(Symbol pkg)
  {
    return Optional.ofNullable(packages.get(pkg));
  }
  
  public static Package getOrCreatePackage(Symbol pkg)
  {
    return packages.computeIfAbsent(pkg, (s) -> new Package(s));
  }
  
  public static void addPackage(Package pkg)
  {
    packages.put(pkg.name(), pkg);
  }
  
  public static Optional<Macro> lookupMacro(Symbol name)
  {
    return Optional.ofNullable(macros.get(name));
  }
  
  public static void defMacro(Symbol name, Macro val)
  {
    macros.put(name, val);
  }
  
  public static void boot()
  {
    var corePkg = CorePkg.getInstance();
    addPackage(corePkg);
    addPackage(new VectorPkg());
    addPackage(new StringPkg());
    addPackage(new PortPkg());
    currentPackage(corePkg);
    Loader.load(Config.BUILTINS_FILE_PATH);
    var userPkg = getOrCreatePackage(Symbol.of("user"));
    userPkg.importUnchecked(corePkg);
    currentPackage(userPkg);
  }
  
  private Runtime() { }
  
  private static final Map<Symbol, Package> packages = new IdentityHashMap<>();
  private static Package currentPackage;
}
