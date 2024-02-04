package spartan.runtime;

import spartan.data.Symbol;
import spartan.errors.SourceInfo;
import spartan.errors.NoSuchPackage;
import spartan.errors.UnboundSymbol;

public final class LoadGlobal extends Inst
{
  public LoadGlobal(Symbol packageName, Symbol baseName, SourceInfo source, Inst next)
  {
    super(next);
    this.packageName = packageName;
    this.baseName = baseName;
    this.source = source;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = spartan.Runtime.getPackage(packageName)
                .orElseThrow(() -> new NoSuchPackage(packageName, source))
                .lookup(baseName)
                .flatMap(value -> value.left())
                .orElseThrow(() -> new UnboundSymbol(packageName, baseName, source));
    vm.control = next;
  }
  
  private final Symbol packageName;
  private final Symbol baseName;
  private final SourceInfo source;
}
