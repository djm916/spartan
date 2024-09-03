package spartan.data;

import spartan.runtime.Kon;

public record Kontinue(Kon kon) implements Datum, IFun
{
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public boolean accepts(int numArgs)
  {
    return numArgs == 1;
  }
}
