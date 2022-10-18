package spartan.data;

import spartan.runtime.VirtualMachine;

public interface Callable
{
  void apply(VirtualMachine vm);
  boolean arityMatches(int numArgs);
}
