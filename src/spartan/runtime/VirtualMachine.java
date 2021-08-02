package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.errors.RuntimeError;

public final class VirtualMachine
{
  Datum result;
  List args = List.Empty;
  Inst control;
  LocalEnv locals;
  GlobalEnv globals;
  Frame frame;
  
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
  
  final void pushFrame(Inst returnTo)
  {
    ++frameCount;
    frame = new Frame(frame, locals, args, returnTo);
    args = List.Empty;
  }
  
  final void popFrame()
  {
    control = frame.returnTo;
    locals = frame.locals;
    args = frame.args;
    frame = frame.parent;
  }
}
