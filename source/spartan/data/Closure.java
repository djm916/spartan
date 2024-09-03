package spartan.data;

import spartan.runtime.Env;
import spartan.runtime.Inst;

public record Closure(Inst body, Signature sig, Env env) implements Datum, IFun
{
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public boolean accepts(int numArgs)
  {
    return sig.matches(numArgs);
  }
}
