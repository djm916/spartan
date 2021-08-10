package spartan.data;

import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

public interface Callable
{
  public void apply(VirtualMachine vm, int numArgs) throws Error;
}
