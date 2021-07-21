package spartan.data;

import spartan.errors.Error;
import spartan.runtime.VirtualMachine;

public abstract class PrimFun extends Datum
{
  public final int numArgs;
  
  protected PrimFun(int numArgs)
  {
    this.numArgs = numArgs;
  }
  
  public final Type type()
  {
    return Type.PrimFun;
  }
  
  public final String repr()
  {
    return Type.PrimFun.name;
  }
  
  public abstract Datum apply(VirtualMachine vm) throws Error;
}
