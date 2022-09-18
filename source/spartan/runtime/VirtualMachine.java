package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Callable;
import spartan.GlobalEnv;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.TypeMismatch;
import spartan.parsing.Position;


public final class VirtualMachine
{
  public Datum result;
  public List args = List.EMPTY;
  public Inst control;
  public LocalEnv locals;
  public GlobalEnv globals;
  public Frame frame;
  
  public VirtualMachine(GlobalEnv globals)
  {
    this.globals = globals;
  }
  
  public final Datum eval(Inst code)
  {
    control = code;
    
    try {
      while (control != null)
        control.eval(this);
    }
    catch (Error err) {
      err.setBackTrace(generateBackTrace());
      reset();
      throw err;
    }
    
    // Assert VM returns to its default state
    assert control == null;
    assert args == List.EMPTY;
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
    args = List.EMPTY;
    return x;
  }
  
  public final void pushFrame(Inst returnTo, Position position)
  {
    frame = new Frame(frame, locals, args, returnTo, position);
    args = List.EMPTY;
  }
  
  public final void popFrame()
  {
    control = frame.returnTo();
    locals = frame.locals();
    args = frame.args();
    frame = frame.parent();
  }
  
  public final void apply(int numArgs)
  {
    if (! (result instanceof Callable))
      throw new TypeMismatch();
    var f = (Callable) result;
    if (!f.matchArity(numArgs))
      throw new WrongNumberArgs();
    f.apply(this);
  }
  
  public final void reset()
  {
    control = null;
    args = List.EMPTY;
    locals = null;
    frame = null;
  }
  
  private java.util.List<Position> generateBackTrace()
  {
    var backTrace = new java.util.ArrayList<Position>();
    for (; frame != null; frame = frame.parent())
      if (frame.position() != null)
        backTrace.add(frame.position());
    return backTrace;
  }
}
