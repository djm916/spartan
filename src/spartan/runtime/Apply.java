package spartan.runtime;

import spartan.data.PrimFun;
import spartan.data.Closure;
import spartan.data.Value;
import spartan.Position;
import spartan.errors.RuntimeError;
import spartan.errors.Error;

public class Apply extends Inst
{
  private final Position position;
  
  public Apply(Position position)
  {
    super(null);
    this.position = position;
  }
  
  public void exec(VirtualMachine vm) throws RuntimeError
  {
    final Value fun = vm.args.pop();
    
    switch (fun.type()) {
      case PrimFun: {
        try {
          vm.result = ((PrimFun)fun).apply(vm);
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
        final Closure clo = (Closure)fun;
        vm.locals = clo.locals;
        vm.control = clo.code;
        break;
      }
      default:
        throw new RuntimeError("attempted application of non-function type", position);
    }
  }
}
