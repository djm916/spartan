package spartan.runtime;

import spartan.data.Value;
import spartan.data.Tuple;

public class MakeTuple extends Inst
{
  private final int numElems;
  
  public MakeTuple(int numElems, Inst next)
  {
    super(next);
    this.numElems = numElems;
  }
  
  public void exec(VirtualMachine vm)
  {
    Value[] elems = new Value[numElems];
    for (int i = numElems - 1; i >= 0; --i)
      elems[i] = vm.args.pop();
    vm.result = new Tuple(elems);
    vm.control = next;
  }
}
