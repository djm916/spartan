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

  public void emit(StringBuilder sb)
  {
    sb.append("(store-local 0 " + offset + ")\n");
    //next.emit(sb);
  }
  
  private final int offset;
}
