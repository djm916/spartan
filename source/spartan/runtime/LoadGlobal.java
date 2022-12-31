package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol name, Position position, Inst next)
  {
    super(next);
    this.name = name;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    try {
      vm.result = GlobalEnv.lookupOrThrow(name);
      vm.control = next;
    }
    catch (UnboundVariable err) {
      err.setPosition(position);
      throw err;
    }
  }

  private final Symbol name;
  private final Position position;
}
