package spartan.parsing;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.data.Value;

public class PositionMap
{
  private final Map<Value, Position> map = new IdentityHashMap<>();
  
  public void put(Value v, Position p)
  {
    map.put(v, p);
  }
  
  public Position get(Value v)
  {
    return map.get(v);
  }
}
