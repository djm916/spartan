package spartan.parsing;

import java.util.Map;
import spartan.Position;
import spartan.data.Value;

public class SourceValue
{
  public final Value value;
  public final Map<Value, Position> positionMap;
  
  SourceValue(Value value, Map<Value, Position> positionMap)
  {
    this.value = value;
    this.positionMap = positionMap;
  }
}
