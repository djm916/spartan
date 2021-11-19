package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;

public class Macro extends Callable
{
  private final Inst code;
  
  public Macro(Inst code, int requiredArgs, boolean isVariadic)
  {
    super(requiredArgs, isVariadic);
    this.code = code;
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
