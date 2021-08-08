package spartan.data;

import java.util.Map;
import java.util.HashMap;

public class Symbol extends Datum
{
  private Symbol(String value)
  {
    this.value = value;
  }
  
  public static Symbol get(String value)
  {
    if (!interned.containsKey(value)) {
      interned.put(value, new Symbol(value));
    }
    return interned.get(value);
  }
  
  public final Type type()
  {
    return Type.Symbol;
  }
  
  public final String repr()
  {
    return "\'" + value;
  }
  
  public final boolean eq(Symbol that)
  {
    return this.value.equals(that.value);
  }
  
  private static Map<String, Symbol> interned = new HashMap<>();
  private final String value;
}
