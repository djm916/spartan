package spartan.data;

import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

public abstract class Callable extends Datum
{
  private final int requiredArgs;
  private final boolean isVariadic;
  
  protected Callable(int requiredArgs, boolean isVariadic)
  {
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  public final boolean matchArity(int numArgs)
  {
    return !isVariadic ? numArgs == requiredArgs
                       : numArgs >= requiredArgs;
  }
  
  public abstract void apply(VirtualMachine vm) throws Error;
}
