package spartan;

import spartan.builtins.CoreNS;
import spartan.builtins.ListNS;
import spartan.builtins.VectorNS;
import spartan.builtins.StringNS;
import spartan.builtins.PortNS;
import spartan.builtins.TableNS;
import spartan.data.Symbol;
import spartan.data.Datum;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public final class Runtime
{
  public static Namespace currentNS()
  {
    return currentNS;
  }
  
  public static void currentNS(Namespace ns)
  {
    currentNS = ns;
  }
  
  public static void enterNS(Symbol ns)
  {
    currentNS = getOrCreateNS(ns);
  }
  
  public static Optional<Namespace> getNS(Symbol ns)
  {
    return Optional.ofNullable(nameSpaces.get(ns));
  }
  
  public static Namespace getOrCreateNS(Symbol ns)
  {
    return nameSpaces.computeIfAbsent(ns, (name) -> new Namespace(name, CoreNS.getInstance()));
  }
  
  public static void addNS(Namespace ns)
  {
    nameSpaces.put(ns.name(), ns);
  }
    
  public static void boot()
  {
    var coreNS = CoreNS.getInstance();
    addNS(coreNS);
    
    
    //addNS(new ListNS());
    addNS(new VectorNS());
    addNS(new StringNS());
    addNS(new TableNS());
    //addNS(new PortNS());
    currentNS(coreNS);
    Loader.load(Config.BUILTINS_FILE_PATH);
    var userNS = new Namespace(Symbol.of("user"), coreNS);
    //userNS.importAllFrom(coreNS);
    addNS(userNS);    
    currentNS(userNS);
  }
  
  private Runtime() { }
  
  private static final Map<Symbol, Namespace> nameSpaces = new IdentityHashMap<>();
  private static Namespace currentNS;
}
