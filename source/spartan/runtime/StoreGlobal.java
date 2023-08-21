package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol nameSpace, Symbol bareName, Position position, Inst next)
  {
    super(next);
    this.nameSpace = nameSpace;
    this.bareName = bareName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    spartan.Runtime.getNS(nameSpace)
                   .orElseThrow(() -> new UnboundVariable(nameSpace, bareName, position))
                   .bind(bareName, vm.result);
    vm.control = next;
  }

  private final Symbol nameSpace;
  private final Symbol bareName;
  private final Position position;
}
