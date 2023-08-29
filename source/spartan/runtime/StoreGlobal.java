package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class StoreGlobal extends Inst
{  
  public StoreGlobal(Symbol packageName, Symbol bareName, Position position, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.bareName = bareName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    spartan.Runtime.getPackage(packageName)
                   .orElseThrow(() -> new UnboundVariable(packageName, bareName, position))
                   .bind(bareName, vm.result);
    vm.control = next;
  }

  private final Symbol packageName;
  private final Symbol bareName;
  private final Position position;
}
