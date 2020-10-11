package spartan.runtime;

import spartan.data.Value;
import spartan.data.Bool;
import spartan.Position;
import spartan.errors.RuntimeError;

public class Branch extends Inst
{
  private final Position position;
  private final Inst ifTrue;
  
  public Branch(Position position, Inst ifTrue, Inst ifFalse)
  {
    super(ifFalse);
    this.position = position;
    this.ifTrue = ifTrue;
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
