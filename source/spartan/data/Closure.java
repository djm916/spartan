package spartan.data;

import spartan.runtime.Env;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.errors.SourceInfo;
import spartan.errors.WrongNumberArgs;

/**
   A closure is an anonymous procedure object.
   
   @param body the instructions that make up the procedure's body
   @param sig the procedure's signature
   @param env the environment current when the procedure was created
*/
public record Closure(Inst body, Signature sig, Env env) implements Datum, IFun
{
  public Datum apply(VirtualMachine vm, List args, SourceInfo source)
  {
    int numArgs = args.length();
    if (!sig.matches(numArgs))
      throw new WrongNumberArgs(source);
    vm.pushFrame(null, source.position());
    vm.args = args;
    vm.env = env;
    vm.eval(body);
    return vm.result;
  }
}
