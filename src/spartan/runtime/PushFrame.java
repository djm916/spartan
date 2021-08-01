package spartan.runtime;

import spartan.data.List;

public class PushFrame extends Inst
{
  private final Inst returnTo;
  
  public PushFrame(Inst returnTo, Inst next)
  {
    super(next);
    this.returnTo = returnTo;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.frame = new Frame(vm.frame, vm.locals, vm.args, returnTo);
    ++vm.frameCount;
    vm.args = List.Empty;
    vm.control = next;
  }
}
