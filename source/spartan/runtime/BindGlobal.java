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

  public void emit(StringBuilder sb)
  {
    sb.append("(bind-global " + packageName.repr() + ":" + baseName.repr() + ")\n");
    //next.emit(sb);
  }
  
  private final Symbol packageName;
  private final Symbol baseName;
  private final SourceInfo source;
}
