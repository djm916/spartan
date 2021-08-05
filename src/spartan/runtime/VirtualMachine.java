package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.PrimFun;
import spartan.data.Closure;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.WrongNumberArgs;

public final class VirtualMachine
{
  public Datum result;
  public List args = List.Empty;
  public Inst control;
  public LocalEnv locals;
  public GlobalEnv globals;
  public Frame frame;
  
  // Metrics
  int frameCount = 0;
  
  public VirtualMachine(GlobalEnv globals)
  {
    this.globals = globals;
  }
  
  public final Datum exec(Inst code) throws RuntimeError
  {
    frameCount = 0;
    
    control = code;
    
    while (control != null)
      control.exec(this);
    
    return result;
  }
  
  public final void pushArg(Datum x)
  {
    args = new List(x, args);
  }
  
  public final Datum popArg()
  {
    Datum x = args.first;
    args = args.rest;
    return x;
  }
  
  public final List popArgs()
  {
    List x = args;
    args = List.Empty;
    return x;
  }
  
  public final void pushFrame(Inst returnTo)
  {
    ++frameCount;
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
    switch (result.type()) {
      case PrimFun: {
        final PrimFun prim = (PrimFun)result;
        if (numArgs < prim.requiredArgs || !prim.isVariadic && numArgs > prim.requiredArgs)
          throw new WrongNumberArgs();
        try {
          result = prim.apply(this);
        }
        finally {
          popFrame();
        }
        break;
      }
      case Closure: {
        final Closure clo = (Closure)result;
        if (numArgs < clo.requiredArgs || !clo.isVariadic && numArgs > clo.requiredArgs)
          throw new WrongNumberArgs();
        locals = clo.locals;
        control = clo.code;
        break;
      }
      default:
        throw new TypeMismatch();
    }
  }
  /*
  public final Datum apply(Datum f, List args) throws Error
  {
    result = f;
    this.args = args;
    apply(args.length());
    return result;
  }
  */
}
