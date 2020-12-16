package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;

public class Closure extends Value
{
  public final Inst code;
  public final LocalEnv locals;
  
  public Closure(Inst code, LocalEnv locals)
  {
    this.code = code;
    this.locals = locals;
  }
  
  public Type type()
  {
    return Type.Closure;
  }
  
  public String repr()
  {
    return String.format("%s @ 0x%x",
      Type.Closure.name, System.identityHashCode(this));
  }
}
