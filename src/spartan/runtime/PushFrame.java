package spartan.runtime;

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
    vm.frame = new Frame(vm.frame, vm.locals, returnTo);
    ++vm.frameCount;
    vm.control = next;
  }
}
