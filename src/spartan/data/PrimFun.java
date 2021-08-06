package spartan.data;

import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.runtime.VirtualMachine;

public abstract class PrimFun extends Datum implements Callable
{
  private final int requiredArgs;
  private final boolean isVariadic;
  
  protected PrimFun(int requiredArgs, boolean isVariadic)
  {
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  public final Type type()
  {
    return Type.PrimFun;
  }
  
  public final String repr()
  {
    return Type.PrimFun.name;
  }
  
  public final void apply(VirtualMachine vm, int numArgs) throws Error
  {
    if (numArgs < requiredArgs || !isVariadic && numArgs > requiredArgs)
      throw new WrongNumberArgs();
    doApply(vm);
  }
  
  public abstract void doApply(VirtualMachine vm) throws Error;
}
