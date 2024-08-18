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
  
  public void emit(StringBuilder sb)
  {
    //sb.append("(branch ? ?)\n");
    //ifTrue.emit(sb);
    //ifFalse.emit(sb);
  }
  
  public final Inst ifTrue;
  public final Inst ifFalse;
}
