package spartan.data;

import java.util.Map;
import java.util.HashMap;

public final class Symbol extends Datum
{
  public static Symbol get(String id)
  {
    if (!interned.containsKey(id))
      interned.put(id, new Symbol(id));
    return interned.get(id);
  }
  
  public static Symbol gen()
  {
    return get(String.format("#%d", nextSymbolNum++));
  }
  
  public Type type()
  {
    return Type.Symbol;
  }
  
  public String repr()
  {
    return id;
  }
    
  private Symbol(String id)
  {
    this.id = id;
  }
  
  private static Map<String, Symbol> interned = new HashMap<>();
  private static int nextSymbolNum;
  
  private final String id;
}
