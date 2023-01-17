package spartan.data;

import spartan.runtime.VirtualMachine;

public interface Callable extends Datum
{
  void apply(VirtualMachine vm);
  boolean arityMatches(int numArgs);
}
