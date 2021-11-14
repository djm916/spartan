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
  
  public Type type()
  {
    return Type.Symbol;
  }
  
  public String repr()
  {
    return id;
  }
  
  public boolean eq(Symbol that)
  {
    return this.id.equals(that.id);
  }
  
  public String toString()
  {
    return id;
  }
  
  private Symbol(String id)
  {
    this.id = id;
  }
  
  private static Map<String, Symbol> interned = new HashMap<>();
  private final String id;
}
