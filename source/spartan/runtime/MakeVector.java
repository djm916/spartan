package spartan.runtime;

import spartan.data.Vector;

public final class MakeVector extends Inst
{
  public MakeVector(Inst next)
  {
    super(next);
  }
  
  public final void eval(VirtualMachine vm)
  {
    vm.result = Vector.fromList(vm.popRestArgs());
    vm.control = next;
  }
}
