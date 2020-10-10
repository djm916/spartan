package spartan.runtime;

import spartan.data.Value;
import spartan.data.Bool;
import spartan.Position;
import spartan.errors.RuntimeError;

public class Branch extends Inst
{
  private final Inst ifTrue;
  private final Position position;
  
  public Branch(Inst ifTrue, Inst ifFalse, Position position)
  {
    super(ifFalse);
    this.ifTrue = ifTrue;
    this.position = position;
  }
  
  public void exec(VirtualMachine vm) throws RuntimeError
  {
    if (vm.result == Bool.True)
      vm.control = ifTrue;
    else if (vm.result == Bool.False)
      vm.control = next;
    else
      throw new RuntimeError("condition must be boolean", position);
  }
}
