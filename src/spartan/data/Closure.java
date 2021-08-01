package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;

public class Closure extends Datum
{
  public final Inst code;
  public final LocalEnv locals;
  public final int requiredArgs;
  public final boolean isVariadic;
  
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
    return String.format("%s @ 0x%x",
      Type.Closure.name, System.identityHashCode(this));
  }
}
