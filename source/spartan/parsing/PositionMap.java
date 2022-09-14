package spartan.parsing;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.data.Datum;

public class PositionMap
{  
  public void put(Datum x, Position p)
  {
    map.put(x, p);
  }
  
  public Position get(Datum x)
  {
    return map.get(x);
  }
  
  public void clear()
  {
    map.clear();
  }
  
  private final Map<Datum, Position> map = new IdentityHashMap<>();
}
