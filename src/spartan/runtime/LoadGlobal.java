package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  private final Symbol symb;
  private final Position position;
  
  public LoadGlobal(Symbol symb, Position position, Inst next)
  {
    super(next);
    this.symb = symb;
    this.position = position;
  }
  
  public void exec(VirtualMachine vm) throws UnboundVariable
  {
    vm.result = vm.globals.lookup(symb);
    if (vm.result == null)
      throw new UnboundVariable(symb, position);
    vm.control = next;
  }
}
