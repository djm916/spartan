package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol packageName, Symbol bareName, Position position, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.bareName = bareName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = spartan.Runtime.getPackage(packageName)
                .flatMap(ns -> ns.lookup(bareName))
                .orElseThrow(() -> new UnboundVariable(packageName, bareName, position));
    vm.control = next;
  }
  
  private final Symbol packageName;
  private final Symbol bareName;
  private final Position position;
}
