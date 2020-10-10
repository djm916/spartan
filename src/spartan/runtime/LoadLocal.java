package spartan.runtime;

import spartan.data.Value;

public class LoadLocal extends Inst
{
  private final int depth;
  
  public LoadLocal(int depth, Inst next)
  {
    super(next);
    this.depth = depth;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.locals.load(depth);
    vm.control = next;
  }
}
