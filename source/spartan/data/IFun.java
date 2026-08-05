package spartan.data;

import spartan.runtime.VirtualMachine;

/**
 * Interface for "callable" types (procedures, continuations, etc.)
 */
public sealed interface IFun extends Datum
permits Primitive, Closure, Continuation
{
  @Override // Datum
  default Type type()
  {
    return Type.PROCEDURE;
  }
  
  Signature sig();
}
