package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.UnboundVariable;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol nameSpace, Symbol baseName, Position position, Inst next)
  {
    super(next);
    this.nameSpace = nameSpace;
    this.baseName = baseName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = spartan.Runtime.getNS(nameSpace)
                .flatMap(ns -> ns.lookup(baseName))
                .orElseThrow(() -> new UnboundVariable(nameSpace, baseName, position));
    vm.control = next;
  }
  
  private final Symbol nameSpace;
  private final Symbol baseName;
  private final Position position;
}
