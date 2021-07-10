package spartan.parsing;

import spartan.data.Value;

public class SourceValue
{
  public final Value value;
  public final PositionMap positionMap;
  
  SourceValue(Value value, PositionMap positionMap)
  {
    this.value = value;
    this.positionMap = positionMap;
  }
}
