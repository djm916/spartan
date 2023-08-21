package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(String nameSpace, Symbol bareName, Position position, Inst next)
  {
    super(next);
    this.nameSpace = nameSpace;
    this.bareName = bareName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = spartan.Runtime.getNS(nameSpace)
                .flatMap(ns -> ns.lookup(bareName))
                .orElseThrow(() -> new UnboundVariable(nameSpace, bareName, position));
    vm.control = next;
  }
  
  private final String nameSpace;
  private final Symbol bareName;
  private final Position position;
}
