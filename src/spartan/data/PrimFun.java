package spartan.data;

import spartan.errors.Error;
import spartan.runtime.VirtualMachine;

public abstract class PrimFun extends Value
{
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
