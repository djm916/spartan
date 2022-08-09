package spartan.data;

import spartan.runtime.Frame;
import spartan.runtime.VirtualMachine;

public final class Continuation extends Callable
{
  private final Frame frame;
  
  public Continuation(Frame frame)
  {
    super(1, false);
    this.frame = frame;
  }
  
  public Type type()
  {
    return Type.Continuation;
  }
  
  public void apply(VirtualMachine vm)
  {    
    vm.frame = frame;
    vm.result = vm.popArg();
    vm.popFrame();
  }
}
