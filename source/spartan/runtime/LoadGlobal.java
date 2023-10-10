package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.NoSuchPackage;
import spartan.errors.UnboundSymbol;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol packageName, Symbol baseName, Position position, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.baseName = baseName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = spartan.Runtime.getPackage(packageName)
                .orElseThrow(() -> new NoSuchPackage(packageName, position))
                .lookup(baseName)
                .flatMap(value -> value.left())
                .orElseThrow(() -> new UnboundSymbol(packageName, baseName, position));
    vm.control = next;
  }
  
  private final Symbol packageName;
  private final Symbol baseName;
  private final Position position;
}
