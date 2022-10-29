package spartan.runtime;

import spartan.parsing.Position;
import spartan.errors.Error;

/**
 * Implements application of a {@link spartan.data.Callable}.
 */
public final class Apply extends Inst
{
  /**
   * @param numArgs the number of arguments being applied
   * @param position the source position where this application occurs
   */
  public Apply(int numArgs, Position position)
  {
    super(null);
    this.numArgs = numArgs;
    this.position = position;
  }
  
  @Override
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
