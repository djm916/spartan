package spartan.runtime;

import spartan.data.Value;

public class LoadGlobal extends Inst
{
  private final int depth;
  
  public LoadGlobal(int depth, Inst next)
  {
    super(next);
    this.depth = depth;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.globals.load(depth);
    vm.control = next;
  }
}
