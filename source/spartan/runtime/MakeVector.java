package spartan.runtime;

import spartan.data.Vector;
import spartan.data.Nil;

public final class MakeVector extends Inst
{
  public MakeVector(int numElems, Inst next)
  {
    super(next);
    this.numElems = numElems;
  }
  
  public final void eval(VirtualMachine vm)
  {
    var v = Vector.create(numElems, Nil.Value);
    for (int i = numElems - 1; i >= 0; --i)
      v.set(i, vm.popArg());
    vm.result = v;
    vm.control = next;
  }
  
  private final int numElems;
}
