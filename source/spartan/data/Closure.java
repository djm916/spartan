package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;

public class Closure extends Datum implements Callable
{
  private final Inst code;
  private final LocalEnv locals;
  private final int requiredArgs;
  private final boolean isVariadic;
  
  public Closure(Inst code, LocalEnv locals, int requiredArgs, boolean isVariadic)
  {
    this.code = code;
    this.locals = locals;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
  
  public final Type type()
  {
    return Type.Closure;
  }
  
  public final String repr()
  {
    return String.format("%s @ 0x%x", Type.Closure.name, System.identityHashCode(this));
  }
  
  public void apply(VirtualMachine vm, int numArgs) throws Error
  {
    if (numArgs < requiredArgs || !isVariadic && numArgs > requiredArgs)
      throw new WrongNumberArgs();
    vm.locals = locals;
    vm.control = code;
  }
}
