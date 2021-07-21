package spartan.runtime;

import spartan.data.Datum;
import spartan.data.Record;

public class MakeRecord extends Inst
{
  private final String[] labels;
  
  public MakeRecord(String[] labels, Inst next)
  {
    super(next);
    this.labels = labels;
  }
  
  public void exec(VirtualMachine vm)
  {
    Datum[] values = new Datum[labels.length];
    for (int i = labels.length - 1; i >= 0; --i)
      values[i] = vm.args.pop();
    vm.result = new Record(labels, values);
    vm.control = next;
  }
}
