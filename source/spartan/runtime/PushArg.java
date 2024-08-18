package spartan.runtime;

public final class PushArg extends Inst
{
  public PushArg(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.pushArg(vm.result);
    vm.control = next;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(push-arg)\n");
    //next.emit(sb);
  }
}
