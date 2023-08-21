package spartan;

import spartan.builtins.CoreNS;
import spartan.data.Datum;
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
  
  private Runtime() { }
  
  private static final Map<Symbol, NameSpace> nameSpaces = new IdentityHashMap<>();
  
  //private static NameSpace coreNS = new CoreLib();
  //private static NameSpace userNS = new NameSpace("user");
  private static NameSpace currentNS;
  
  static
  {
    var coreNS = new CoreNS();
    nameSpaces.put(coreNS.name(), coreNS);
    var userNS = new NameSpace(Symbol.of("user"), coreNS);
    nameSpaces.put(userNS.name(), userNS);
    //userNS.importAll(coreNS);
    currentNS = userNS;
  }
}
