package spartan.data;

import spartan.errors.Error;
import spartan.runtime.VirtualMachine;

public abstract class PrimFun extends Value
{
  public final int numArgs;
  
  protected PrimFun(int numArgs)
  {
    this.numArgs = numArgs;
  }
  
  public Type type()
  {
    return Type.PrimFun;
  }
  
  public String repr()
  {
    return Type.PrimFun.name;
  }
  
  public abstract Value apply(VirtualMachine vm) throws Error;
}
