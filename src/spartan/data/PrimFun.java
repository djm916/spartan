package spartan.data;

import spartan.errors.Error;
import spartan.runtime.VirtualMachine;

public abstract class PrimFun extends Datum
{
  public final int requiredArgs;
  public final boolean isVariadic;
  
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
  
  public abstract void apply(VirtualMachine vm) throws Error;
}
