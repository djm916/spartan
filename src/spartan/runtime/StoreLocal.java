package spartan.runtime;

import spartan.data.Value;

public class StoreLocal extends Inst
{
  private final int depth;
  
  public StoreLocal(int depth, Inst next)
  {
    super(next);
    this.depth = depth;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.locals.store(vm.result, depth);
    vm.control = next;
  }
}
