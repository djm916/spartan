package spartan.data;

import spartan.errors.Error;
import spartan.errors.TypeMismatch;

public class RecordSelector extends PrimFun
{
  private final String label;
  
  public RecordSelector(String label)
  {
    this.label = label;
  }
  
  public Value apply(Value arg) throws Error
  {
    if (arg.type() != Type.Record)
      throw new TypeMismatch();
    return ((Record)arg).at(label);
  }
}
