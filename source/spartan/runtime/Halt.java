package spartan.runtime;

public final class Halt extends Inst
{
  public Halt()
  {
    super(null);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.control = null;
  }
  
  public void emit(StringBuilder sb)
  {
    sb.append("(halt)\n");
  }  
}
