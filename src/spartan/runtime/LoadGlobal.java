package spartan.runtime;

import spartan.data.Value;
import spartan.Position;
import spartan.errors.UnboundVariable;

public class LoadGlobal extends Inst
{
  private final Position position;
  private final String id;
  
  public LoadGlobal(Position position, String id, Inst next)
  {
    super(next);
    this.position = position;
    this.id = id;
  }
  
  public void exec(VirtualMachine vm) throws UnboundVariable
  {    
    vm.result = vm.globals.lookup(id);
    if (vm.result == null)
      throw new UnboundVariable(id, position);
    vm.control = next;
  }
}
