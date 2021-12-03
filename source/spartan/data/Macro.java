package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.compiling.ProcTemplate;

public class Macro extends Callable
{
  private final Inst code;
  
  public Macro(ProcTemplate template)
  {
    super(template.requiredArgs, template.isVariadic);
    this.code = template.code;
  }
  
  public Type type()
  {
    return Type.Macro;
  }
  
  public void apply(VirtualMachine vm)
  {
    vm.control = code;
  }
}
