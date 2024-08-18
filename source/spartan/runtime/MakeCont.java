package spartan.runtime;

import spartan.data.Continuation;

public final class MakeCont extends Inst
{
  public MakeCont(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = new Continuation(vm.frame);
    vm.control = next;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(make-cont)\n");
    //next.emit(sb);
  }
}
