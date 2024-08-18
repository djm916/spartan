package spartan.runtime;

public final class PopRestArgs extends Inst
{
  public PopRestArgs(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = vm.popRestArgs();
    vm.control = next;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(pop-arg*)");
    //next.emit(sb);
  }
}
