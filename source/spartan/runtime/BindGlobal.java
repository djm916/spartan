package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;
import spartan.errors.NoSuchPackage;
import spartan.errors.MultipleDefinition;

public final class BindGlobal extends Inst
{  
  public BindGlobal(Symbol packageName, Symbol baseName, SourceInfo source, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.baseName = baseName;
    this.source = source;
  }
  
  public final void eval(VirtualMachine vm)
  {
    spartan.Runtime.getPackage(packageName)
                   .orElseThrow(() -> new NoSuchPackage(packageName, source))
                   .bind(baseName, vm.result, () -> new MultipleDefinition(baseName, source));
    vm.control = next;
  }
  
  final Symbol packageName;
  final Symbol baseName;
  final SourceInfo source;
}
