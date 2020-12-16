package spartan.runtime;

import spartan.data.Value;
import spartan.data.List;

public class MakeList extends Inst
{
  private final int numElems;
  
  public MakeList(int numElems, Inst next)
  {
    super(next);
    this.numElems = numElems;
  }
  
  public void exec(VirtualMachine vm)
  {
    List list = List.Empty;
    for (int i = 0; i < numElems; ++i)
      list = new List(vm.args.pop(), list);
    vm.result = list;
    vm.control = next;
  }
}
