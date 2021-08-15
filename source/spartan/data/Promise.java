package spartan.data;

import spartan.runtime.Inst;
import spartan.runtime.LocalEnv;

public class Promise extends Closure implements Callable
{
  public Promise(Inst code, LocalEnv locals)
  {
    super(code, locals, 0, false);
  }
  
  public final Type type()
  {
    return Type.Promise;
  }
  
  public final String repr()
  {
    return String.format("%s @ 0x%x", type().name, System.identityHashCode(this));
  }  
}
