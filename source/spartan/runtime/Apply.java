package spartan.runtime;

import spartan.errors.SourceInfo;
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
  public Apply(int numArgs, SourceInfo source)
  {
    super(null);
    this.numArgs = numArgs;
    this.source = source;
  }
  
  @Override
  public void eval(VirtualMachine vm)
  {
    try {
      vm.apply(numArgs);
    }
    catch (Error err) {
      err.setSource(source);
      throw err;
    }
  }

  private final int numArgs;
  private final SourceInfo source;
}
