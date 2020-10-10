package spartan.data;

import spartan.errors.Error;
import spartan.errors.TypeMismatch;

public class TupleSelector extends PrimFun
{
  private final int index;
  
  public TupleSelector(int index)
  {
    this.index = index;
  }
  
  public Value apply(Value arg) throws Error
  {
    if (arg.type() != Type.Tuple)
      throw new TypeMismatch();
    return ((Tuple)arg).at(index);
  }
}
