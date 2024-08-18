package spartan.runtime;

public final class PopFrame extends Inst
{
  public PopFrame()
  {
    super(null);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.popFrame();
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(pop-frame)");
    //next.emit(sb);
  }
}
