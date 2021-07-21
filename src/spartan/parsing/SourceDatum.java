package spartan.parsing;

import spartan.data.Datum;

public class SourceDatum
{
  public final Datum datum;
  public final PositionMap positionMap;
  
  SourceDatum(Datum datum, PositionMap positionMap)
  {
    this.datum = datum;
    this.positionMap = positionMap;
  }
}
