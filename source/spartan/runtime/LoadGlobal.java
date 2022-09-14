package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.UnboundVariable;
import spartan.parsing.Position;

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
    var value = vm.globals.lookup(name);
    if (value == null)
      throw new UnboundVariable(name, position);
    vm.result = value;
    vm.control = next;
  }

  private final Symbol name;
  private final Position position;
}
