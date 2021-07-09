package spartan.runtime;

import spartan.data.Value;
import spartan.data.Vector;

public class MakeVector extends Inst
{
  private final int numElems;
  
  public MakeVector(int numElems, Inst next)
  {
    super(next);
    this.numElems = numElems;
  }
  
  public void exec(VirtualMachine vm)
  {
    Value[] elems = new Value[numElems];
    for (int i = numElems - 1; i >= 0; --i)
      elems[i] = vm.args.pop();
    vm.result = new Vector(elems);
    vm.control = next;
  }
}
