package spartan.runtime;

import spartan.data.Symbol;
import spartan.parsing.Position;
import spartan.errors.NoSuchPackage;
import spartan.errors.MultipleDefinition;

public final class BindGlobal extends Inst
{  
  public BindGlobal(Symbol packageName, Symbol baseName, Position position, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.baseName = baseName;
    this.position = position;
  }
  
  public final void eval(VirtualMachine vm)
  {
    spartan.Runtime.getPackage(packageName)
                   .orElseThrow(() -> new NoSuchPackage(packageName, position))
                   .bind(baseName, vm.result, () -> new MultipleDefinition(baseName, position));
    vm.control = next;
  }

  private final Symbol packageName;
  private final Symbol baseName;
  private final Position position;
}
