package spartan.runtime;

public final class LoadLocal extends Inst
{
  public LoadLocal(int depth, int offset, Inst next)
  {
    super(next);
    this.depth = depth;
    this.offset = offset;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.locals.load(depth, offset);
    vm.control = next;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(load-local " + depth + ":" + offset + ")\n");
    //next.emit(sb);
  }
  
  private final int depth;
  private final int offset;
}
