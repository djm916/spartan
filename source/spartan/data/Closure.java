package spartan.data;

import spartan.runtime.Env;
import spartan.runtime.Inst;

/**
   A closure is an anonymous procedure object.
   
   @param body the instructions that make up the procedure's body
   @param sig the procedure's signature
   @param the environment current when the procedure was created
*/
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
