package spartan.data;

import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

public abstract sealed class Callable extends Datum
permits Primitive, Closure, Macro, Continuation
{
  protected Callable(int requiredArgs, boolean isVariadic)
  {
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  public abstract void apply(VirtualMachine vm);
  
  public final boolean arityMatches(int numArgs)
  {
    return !isVariadic ? numArgs == requiredArgs
                       : numArgs >= requiredArgs;
  }
  
  private final int requiredArgs;
  private final boolean isVariadic;
}
