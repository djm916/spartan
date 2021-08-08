package spartan.runtime;

import spartan.Builtins;

public final class Branch extends Inst
{
  private final Inst ifTrue;
  
  public Branch(Inst ifTrue, Inst ifFalse)
  {
    super(ifFalse);
    this.ifTrue = ifTrue;
  }
  
  public void exec(VirtualMachine vm)
  {
    if (Builtins.truth(vm.result))
      vm.control = ifTrue;
    else
      vm.control = next;
  }
}
