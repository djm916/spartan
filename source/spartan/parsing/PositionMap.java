package spartan.parsing;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.data.Datum;

/**
  A source map that provides a source position (filename, line, and column)
  associated with a given source expression (lists and symbols).
*/
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
