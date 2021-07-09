package spartan.data;

import spartan.errors.Error;
import spartan.errors.TypeMismatch;

public class VectorSelector extends PrimFun
{
  private final int index;
  
  public VectorSelector(int index)
  {
    this.index = index;
  }
  
  public Value apply(Value arg) throws Error
  {
    if (arg.type() != Type.Vector)
      throw new TypeMismatch();
    return ((Vector)arg).at(index);
  }
}
