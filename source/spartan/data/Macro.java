package spartan.data;

import spartan.compiling.Procedure;
import spartan.runtime.VirtualMachine;
import spartan.runtime.Inst;
import spartan.errors.SourceInfo;
import spartan.errors.WrongNumberArgs;

public record Macro(Inst body, Signature sig) implements Datum
{
  public Macro(Procedure proc)
  {
    this(proc.body(), proc.sig());
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.MACRO;
  }
  
  public Datum expand(VirtualMachine vm, List args, SourceInfo source)
  {
    int numArgs = args.length();
    if (!sig.matches(numArgs))
      throw new WrongNumberArgs(source);
    vm.pushFrame(null, source.position());
    vm.args = args;
    vm.eval(body);
    return vm.result;
  }
}
