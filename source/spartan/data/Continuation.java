package spartan.data;

import spartan.runtime.Frame;
import spartan.runtime.VirtualMachine;

public final class Continuation implements Datum, Callable
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
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return numArgs == 1;
  }
  
  private final Frame savedFrame;
}
