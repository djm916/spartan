package spartan.runtime;

public final class BranchFalse extends Inst
{  
  public BranchFalse(Inst left, Inst right)
  {
    super(left);
    this.alt = right;
  }
  
  public void eval(VirtualMachine vm)
  {
    if (!vm.result.boolValue())
      vm.control = alt;
    else
      vm.control = next;
  }
      
  final Inst alt;
}
