package spartan.runtime;

import spartan.data.Value;

public class StoreGlobal extends Inst
{
  private final int depth;
  
  public StoreGlobal(int depth, Inst next)
  {
    super(next);
    this.depth = depth;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.globals.store(vm.result, depth);
    vm.control = next;
  }
}
