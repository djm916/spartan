package spartan.data;

import spartan.runtime.Frame;
import spartan.runtime.VirtualMachine;

public final class Continuation implements Datum, IFun
{
  public Continuation(Frame frame)
  {
    this.savedFrame = frame;
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override
  public void apply(VirtualMachine vm)
  {    
    vm.frame = savedFrame;
    vm.result = vm.popArg();
    vm.popFrame();
  }
  
  @Override // IFun
  public Signature signature()
  {
    return SIG;
  }
  
  private static final Signature SIG = Signature.fixed(1);
  private final Frame savedFrame;
}
