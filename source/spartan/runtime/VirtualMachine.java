package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Callable;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.TypeMismatch;

public final class VirtualMachine
{
  public Datum result;
  public List args = List.Empty;
  public Inst control;
  public LocalEnv locals;
  public GlobalEnv globals;
  public Frame frame;
  
  public VirtualMachine(GlobalEnv globals)
  {
    this.globals = globals;
  }
  
  public final Datum eval(Inst code) throws RuntimeError
  {
    control = code;
    
    try {
      while (control != null)
        control.eval(this);
    }
    catch (RuntimeError ex) {
      reset();
      throw ex;
    }
    
    assert control == null;
    assert args == List.Empty;
    assert locals == null;
    assert frame == null;

    return result;
  }
  
  public final void pushArg(Datum x)
  {
    args = List.cons(x, args);
  }
  
  public final Datum popArg()
  {
    Datum x = args.car();
    args = args.cdr();
    return x;
  }
  
  public final Datum peekArg()
  {
    return args.car();
  }
  
  public final List popRestArgs()
  {
    List x = args;
    args = List.Empty;
    return x;
  }
  
  public final void pushFrame(Inst returnTo)
  {
    frame = new Frame(frame, locals, args, returnTo);
    args = List.Empty;
  }
  
  public final void popFrame()
  {
    control = frame.returnTo;
    locals = frame.locals;
    args = frame.args;
    frame = frame.parent;
  }
  
  public final void apply(int numArgs) throws Error
  {
    if (! (result instanceof Callable))
      throw new TypeMismatch();
    Callable f = (Callable) result;
    if (!f.checkArity(numArgs))
      throw new WrongNumberArgs();
    f.apply(this);
  }
  
  public final void reset()
  {
    control = null;
    args = List.Empty;
    locals = null;
    frame = null;
  }
}
