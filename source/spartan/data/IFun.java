package spartan.data;

import spartan.runtime.VirtualMachine;
import spartan.compiling.Signature;

/**
 * Interface for "callable" types (procedures, continuations, etc.)
 */
public interface IFun extends Datum
{
  void apply(VirtualMachine vm);
  Signature signature();
}
