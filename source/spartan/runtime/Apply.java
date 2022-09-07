package spartan.runtime;

import spartan.parsing.Position;
import spartan.errors.Error;

public final class Apply extends Inst
{  
  public Apply(int numArgs, Position position)
  {
    super(null);
    this.numArgs = numArgs;
    this.position = position;
  }
  
  public void eval(VirtualMachine vm)
  {
    try {
      vm.apply(numArgs);
    }
    catch (Error err) {
      err.setPosition(position);
      throw err;
    }
  }

  private final int numArgs;
  private final Position position;
}
