package spartan.runtime;

public final class PopArg extends Inst
{
  public PopArg(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.popArg();
    vm.control = next;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(pop-arg)\n");
    //next.emit(sb);
  }
}
