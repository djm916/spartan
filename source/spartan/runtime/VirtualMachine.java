package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.IFun;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.TypeMismatch;
import spartan.parsing.Position;

/**
 * Represents state of the virtual machine.
 */
public final class VirtualMachine
{
  /**
   * Value of the last evaluated expression.
   */
  public Datum result;
  
  /**
   * List of procedure arguments.
   */
  public List args = List.EMPTY;
  
  /**
   * The next instruction to evaluate.
   */
  public Inst control;
  
  /**
   * The local environment.
   */
  public LocalEnv locals;
    
  /**
   * The call stack.
   */
  public Frame frame;
    
  public Datum eval(Inst code)
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
  
  public void pushArg(Datum x)
  {
    args = List.cons(x, args);
  }
  
  public Datum popArg()
  {
    Datum x = args.car();
    args = args.cdr();
    return x;
  }
  
  public List popRestArgs()
  {
    List x = args;
    args = List.EMPTY;
    return x;
  }
  
  public void pushFrame(Inst returnTo, Position position)
  {
    frame = new Frame(frame, locals, args, returnTo, position);
    args = List.EMPTY;
  }
  
  public void popFrame()
  {
    control = frame.returnTo();
    locals = frame.locals();
    args = frame.args();
    frame = frame.parent();
  }
  
  public void apply(int numArgs)
  {
    if (!(result instanceof IFun f))
      throw new TypeMismatch();
    if (!f.signature().matches(numArgs))
      throw new WrongNumberArgs();
    f.apply(this);
  }
  
  public void reset()
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
