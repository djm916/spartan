package spartan;

import spartan.builtins.CoreNS;
import spartan.builtins.VectorNS;
import spartan.builtins.StringNS;
import spartan.builtins.PortNS;
import spartan.data.Symbol;
import java.util.Map;
import java.util.IdentityHashMap;
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
    currentNS = getOrCreateNS(ns, currentNS);
  }
  
  public static void leaveNS()
  {
    currentNS = currentNS.parent();
  }
  
  public static Optional<NameSpace> getNS(Symbol ns)
  {
    return Optional.ofNullable(nameSpaces.get(ns));
  }
  
  public static NameSpace getOrCreateNS(Symbol ns, NameSpace parent)
  {
    return nameSpaces.computeIfAbsent(ns, (s) -> new NameSpace(s, parent));
  }
  
  public static void putNS(NameSpace ns)
  {
    nameSpaces.put(ns.name(), ns);
  }
  
  private Runtime() { }
    
  private static final Map<Symbol, NameSpace> nameSpaces = new IdentityHashMap<>();
    
  private static NameSpace currentNS;
  
  static
  {
    var coreNS = CoreNS.getInstance();
    putNS(coreNS);
    putNS(new VectorNS(coreNS));
    putNS(new StringNS(coreNS));
    putNS(new PortNS(coreNS));
    var userNS = new NameSpace(Symbol.of("user"), coreNS);
    putNS(userNS);
    currentNS = userNS;
  }
}
