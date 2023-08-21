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
  
  public static void enterNS(String nsName)
  {
    currentNS = getOrCreateNS(nsName);
  }
  
  public static Optional<NameSpace> getNS(String name)
  {
    return Optional.ofNullable(nameSpaces.get(name));
  }
  
  public static NameSpace getOrCreateNS(String name)
  {
    return nameSpaces.computeIfAbsent(name, (dummy) -> new NameSpace(name));
  }
    
  private Runtime() { }
  
  private static final Map<String, NameSpace> nameSpaces = new IdentityHashMap<>();
  
  //private static NameSpace coreNS = new CoreLib();
  //private static NameSpace userNS = new NameSpace("user");
  private static NameSpace currentNS;
  
  static
  {
    var coreNS = new CoreNS();
    nameSpaces.put(coreNS.name(), coreNS);
    var userNS = new NameSpace("user");
    nameSpaces.put(userNS.name(), userNS);
    userNS.importAll(coreNS);
    currentNS = userNS;
  }
}
