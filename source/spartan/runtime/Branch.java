package spartan.runtime;

import spartan.builtins.CoreLib;

public final class Branch extends Inst
{
  private final Inst ifTrue;
  
  public Branch(Inst ifTrue, Inst ifFalse)
  {
    super(ifFalse);
    this.ifTrue = ifTrue;
  }
  
  public void eval(VirtualMachine vm)
  {
    if (CoreLib.truth(vm.result))
      vm.control = ifTrue;
    else
      vm.control = next;
  }
}
