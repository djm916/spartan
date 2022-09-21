package spartan.data;

import spartan.runtime.Frame;
import spartan.runtime.VirtualMachine;

public final class Continuation extends Callable
{
  public Continuation(Frame frame)
  {
    super(1, false);
    this.frame = frame;
  }
  
  public Type type()
  {
    return Type.CONTINUE;
  }
  
  public void apply(VirtualMachine vm)
  {    
    vm.frame = frame;
    vm.result = vm.popArg();
    vm.popFrame();
  }

  private final Frame frame;
}
