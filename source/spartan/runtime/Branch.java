package spartan.runtime;

import spartan.builtins.CoreLib;

public final class Branch extends Inst
{  
  public Branch(Inst ifTrue, Inst ifFalse)
  {
    super(null);
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }
  
  public void eval(VirtualMachine vm)
  {
    if (CoreLib.truth(vm.result))
      vm.control = ifTrue;
    else
      vm.control = ifFalse;
  }

  private final Inst ifTrue;
  private final Inst ifFalse;
}
