package spartan.data;

import spartan.runtime.Frame;
import spartan.runtime.VirtualMachine;
import spartan.compiling.Signature;

public final class Continuation implements Datum, IFun
{
  public Continuation(Frame frame)
  {
    this.savedFrame = frame;
  }
  
  @Override
  public String type()
  {
    return "procedure";
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
  
  private static final Signature SIG = new Signature(1, false);
  private final Frame savedFrame;
}
