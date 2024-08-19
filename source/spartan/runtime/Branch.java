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
  
  public void emit(StringBuilder sb) {}
  
  final Inst ifTrue;
  final Inst ifFalse;
}
