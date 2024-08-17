package spartan.runtime;

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
    if (vm.result.boolValue())
      vm.control = ifTrue;
    else
      vm.control = ifFalse;
  }
    
  private final Inst ifTrue;
  private final Inst ifFalse;
}
