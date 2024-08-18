package spartan.runtime;

public final class PushEnv extends Inst
{
  public PushEnv(int numSlots, Inst next)
  {
    super(next);
    this.numSlots = numSlots;
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.locals = new LocalEnv(numSlots, vm.locals);
    vm.control = next;
  }

  public void emit(StringBuilder sb)
  {
    sb.append("(push-env " + numSlots + ")\n");
    //next.emit(sb);
  }
  
  private final int numSlots;
}
