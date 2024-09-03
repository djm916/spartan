package spartan.data;

import spartan.runtime.VirtualMachine;

/**
 * Interface for "callable" types (procedures, continuations, etc.)
 */
public sealed interface IFun extends Datum
permits Primitive, Closure, Kontinue
{
  boolean accepts(int numArgs);
}
