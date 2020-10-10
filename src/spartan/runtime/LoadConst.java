package spartan.runtime;

import spartan.data.Value;

public class LoadConst extends Inst
{
  private final Value value;
  
  public LoadConst(Value value, Inst next)
  {
    super(next);
    this.value = value;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = value;
    vm.control = next;
  }
}
