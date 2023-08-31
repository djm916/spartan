package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol nameSpace, Symbol baseName, Position position, Inst next)
  {
    super(next);
    this.nameSpace = nameSpace;
    this.baseName = baseName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    spartan.Runtime.getNS(nameSpace)
                   .orElseThrow(() -> new UnboundVariable(nameSpace, baseName, position))
                   .bind(baseName, vm.result);
    vm.control = next;
  }

  private final Symbol nameSpace;
  private final Symbol baseName;
  private final Position position;
}
