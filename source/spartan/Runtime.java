package spartan;

import spartan.builtins.CoreNS;
import spartan.builtins.ListNS;
import spartan.builtins.VectorNS;
import spartan.builtins.StringNS;
import spartan.builtins.PortNS;
import spartan.data.Symbol;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public final class Runtime
{
  public static Namespace currentNS()
  {
    return currentNS;
  }
  
  public static void currentNS(Namespace pkg)
  {
    currentNS = pkg;
  }
  
  public static void enterNS(Symbol pkg)
  {
    currentNS = getOrCreateNS(pkg);
  }
  
  public static Optional<Namespace> getNS(Symbol pkg)
  {
    return Optional.ofNullable(nameSpaces.get(pkg));
  }
  
  public static Namespace getOrCreateNS(Symbol pkg)
  {
    return nameSpaces.computeIfAbsent(pkg, (s) -> new Namespace(s));
  }
  
  public static void addNS(Namespace pkg)
  {
    nameSpaces.put(pkg.name(), pkg);
  }
    
  public static void boot()
  {
    var coreNS = CoreNS.getInstance();
    addNS(coreNS);
    addNS(new ListNS());
    addNS(new VectorNS());
    addNS(new StringNS());
    addNS(new PortNS());
    currentNS(coreNS);
    Loader.load(Config.BUILTINS_FILE_PATH);
    var userNS = getOrCreateNS(Symbol.of("user"));
    userNS.importUnchecked(coreNS);
    currentNS(userNS);
  }
  
  private Runtime() { }
  
  private static final Map<Symbol, Namespace> nameSpaces = new IdentityHashMap<>();
  private static Namespace currentNS;
}
