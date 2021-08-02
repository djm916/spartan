package spartan.runtime;

import spartan.data.PrimFun;
import spartan.data.Closure;
import spartan.data.Datum;
import spartan.data.List;
import spartan.parsing.Position;
import spartan.errors.RuntimeError;
import spartan.errors.Error;

public final class Apply extends Inst
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
    final Datum fun = vm.result;
    
    switch (fun.type()) {
      case PrimFun: {
        final PrimFun prim = (PrimFun)fun;
        if (numArgs < prim.requiredArgs || !prim.isVariadic && numArgs > prim.requiredArgs)
          throw new RuntimeError("incorrect number of arguments in call", position);
        try {
          vm.result = prim.apply(vm);
        }
        catch (Error err) {
          throw new RuntimeError(err.getMessage(), position);
        }
        finally {
          vm.popFrame();
        }
        break;
      }
      case Closure: {
        final Closure clo = (Closure)fun;
        if (numArgs < clo.requiredArgs || !clo.isVariadic && numArgs > clo.requiredArgs)
          throw new RuntimeError("incorrect number of arguments in call", position);
        vm.locals = clo.locals;
        vm.control = clo.code;
        break;
      }
      default:
        throw new RuntimeError("attempt to call non-function", position);
    }
  }
}
