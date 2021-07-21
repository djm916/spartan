package spartan.runtime;

import spartan.data.PrimFun;
import spartan.data.Closure;
import spartan.data.Datum;
import spartan.parsing.Position;
import spartan.errors.RuntimeError;
import spartan.errors.Error;

public class Apply extends Inst
{
  private final int numArgs;
  private final Position position;
  
  public Apply(int numArgs, Position position)
  {
    super(null);
    this.numArgs = numArgs;
    this.position = position;
  }
  
  public void exec(VirtualMachine vm) throws RuntimeError
  {
    final Datum fun = vm.args.pop();
    
    switch (fun.type()) {
      case PrimFun: {
        final PrimFun primFun = (PrimFun)fun;
        if (numArgs != primFun.numArgs)
          throw new RuntimeError("incorrect number of arguments in call", position);
        try {
          vm.result = primFun.apply(vm);
        }
        catch (Error err) {
          throw new RuntimeError(err.getMessage(), position);
        }
        finally {
          vm.control = vm.frame.returnTo;
          vm.locals = vm.frame.locals;
          vm.frame = vm.frame.parent;
        }
        break;
      }
      case Closure: {
        final Closure closure = (Closure)fun;
        if (numArgs != closure.numArgs)
          throw new RuntimeError("incorrect number of arguments in call", position);
        vm.locals = closure.locals;
        vm.control = closure.code;
        break;
      }
      default:
        throw new RuntimeError("attempt to call non-function", position);
    }
  }
}
