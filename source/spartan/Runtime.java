package spartan;

import spartan.builtins.CoreNS;
import spartan.builtins.ListNS;
import spartan.builtins.VectorNS;
import spartan.builtins.StringNS;
import spartan.builtins.PortNS;
import spartan.data.Symbol;
import spartan.compiling.Macro;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashMap;
import java.util.Optional;

public final class Runtime
{
  public static NameSpace currentNS()
  {
    return currentNS;
  }
  
  public static void currentNS(NameSpace ns)
  {
    currentNS = ns;
  }
  
  public static void enterNS(Symbol ns)
  {
    currentNS = getOrCreateNS(ns);
  }
  
  public static Optional<NameSpace> getNS(Symbol ns)
  {
    return Optional.ofNullable(nameSpaces.get(ns));
  }
  
  public static NameSpace getOrCreateNS(Symbol ns)
  {
    return nameSpaces.computeIfAbsent(ns, (s) -> new NameSpace(s));
  }
  
  public static void putNS(NameSpace ns)
  {
    nameSpaces.put(ns.name(), ns);
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
    // initialize "core" namespace with primitive builtins
    var coreNS = CoreNS.getInstance();
    putNS(coreNS);
    putNS(new VectorNS());
    putNS(new StringNS());
    putNS(new PortNS());
    // initialize "user" namespace
    var userNS = getOrCreateNS(Symbol.of("user"));
    userNS.importUnchecked(coreNS);
    //userNS.importUnchecked(listNS);
    currentNS(userNS);
    //System.out.println("in Runtime.init(), current namespace = " + currentNS().name().repr());    
    Loader.load(Config.BUILTINS_FILE_PATH); 
    userNS.importUnchecked(coreNS);
    currentNS(userNS);
  }
  
  private Runtime() { }
  
  private static final Map<Symbol, NameSpace> nameSpaces = new IdentityHashMap<>();
  
  private static final Map<Symbol, Macro> macros = new HashMap<>();
  
  private static NameSpace currentNS;
  
}
