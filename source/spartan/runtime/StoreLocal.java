package spartan.runtime;

public final class StoreLocal extends Inst
{  
  public StoreLocal(int depth, int offset, Inst next)
  {
    super(next);
    this.depth = depth;
    this.offset = offset;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.locals.store(vm.result, depth, offset);
    vm.control = next;
  }
  
  final int depth;
  final int offset;
}
