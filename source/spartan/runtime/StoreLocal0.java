package spartan.runtime;

public final class StoreLocal0 extends Inst
{  
  public StoreLocal0(int offset, Inst next)
  {
    super(next);
    this.offset = offset;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.locals.store0(vm.result, offset);
    vm.control = next;
  }
  
  final int offset;
}
