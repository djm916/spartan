package spartan.runtime;

public final class LoadLocal0 extends Inst
{
  public LoadLocal0(int offset, Inst next)
  {
    super(next);
    this.offset = offset;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.locals.load0(offset);
    vm.control = next;
  }

  public void emit(StringBuilder sb)
  {
    sb.append("(load-local 0" + offset + ")\n");
    //next.emit(sb);
  }
  
  private final int offset;
}
