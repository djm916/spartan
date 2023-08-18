package spartan.parsing;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.data.Datum;

/**
 * A mapping of {@code Datum}s to their corresponding source {@code Position}s.
 * 
 * Only includes {@code Symbol}s and {@code List} literals returned by {@link Reader#read}.
 */
public class PositionMap
{
  /**
   * Creates a new, empty instance.
   */
  public PositionMap()
  {}
  
  /**
   * Retrieves the source position of the given expression.
   *
   * @return the source position of the given expression, or {@code null} if no position is known
   */
  public Position get(Datum exp)
  {
    return map.get(exp);
  }
  
  /**
   * Sets the position of the given expression.
   */
  public void put(Datum exp, Position pos)
  {
    map.put(exp, pos);
  }
  
  /**
   * Removes all entries.
   */
  public void clear()
  {
    map.clear();
  }
  
  private final Map<Datum, Position> map = new IdentityHashMap<>();
}
