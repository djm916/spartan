package spartan.runtime;

import spartan.data.List;

public final class PopArgs extends Inst
{
  public PopArgs(Inst next)
  {
    super(next);
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.args;
    vm.args = List.Empty;
    vm.control = next;
  }
}
