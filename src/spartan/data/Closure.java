package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;

public class Closure extends Datum
{
  public final Inst code;
  public final int numArgs;
  public final LocalEnv locals;
  
  public Closure(Inst code, int numArgs, LocalEnv locals)
  {
    this.code = code;
    this.numArgs = numArgs;
    this.locals = locals;
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
