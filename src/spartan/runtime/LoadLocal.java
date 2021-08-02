package spartan.runtime;

public final class LoadLocal extends Inst
{
  private final int depth;
  private final int offset;
  
  public LoadLocal(int depth, int offset, Inst next)
  {
    super(next);
    this.depth = depth;
    this.offset = offset;
  }
  
  public void exec(VirtualMachine vm)
  {
    vm.result = vm.locals.load(depth, offset);
    vm.control = next;
  }
}
