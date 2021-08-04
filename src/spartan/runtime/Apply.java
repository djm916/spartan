package spartan.runtime;

import spartan.parsing.Position;
import spartan.errors.RuntimeError;
import spartan.errors.Error;

public final class Apply extends Inst
{
  private final int numArgs;
  private final Position position;
  
  public Apply(int numArgs, Position position)
  {
    super(null);
    this.numArgs = numArgs;
    this.position = position;
  }
  
  public final void exec(VirtualMachine vm) throws RuntimeError
  {
    try {
      vm.apply(numArgs);
    }
    catch (Error err) {
      throw new RuntimeError(err.getMessage(), position);
    }
  }
}
