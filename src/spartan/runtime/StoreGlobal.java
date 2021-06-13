package spartan.runtime;

import spartan.data.Value;

public class StoreGlobal extends Inst
{
  private final String id;
  
  public StoreGlobal(String id, Inst next)
  {
    super(next);
    this.id = id;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.globals.bind(id, vm.result);
    vm.control = next;
  }
}
