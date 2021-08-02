package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  private final Position position;
  private final Symbol symb;
  
  public LoadGlobal(Position position, Symbol symb, Inst next)
  {
    super(next);
    this.position = position;
    this.symb = symb;
  }
  
  public void exec(VirtualMachine vm) throws UnboundVariable
  {
    vm.result = vm.globals.lookup(symb);
    if (vm.result == null)
      throw new UnboundVariable(symb, position);
    vm.control = next;
  }
}
