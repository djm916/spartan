package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.errors.RuntimeError;

public class VirtualMachine
{
  public Datum result;
  public List args = List.Empty;
  public Inst control;
  public LocalEnv locals;
  public GlobalEnv globals;
  public Frame frame;
  
  // Metrics
  public int frameCount = 0;
  
  public VirtualMachine(GlobalEnv globals)
  {
    this.globals = globals;
  }
  
  public Datum exec(Inst code) throws RuntimeError
  {
    frameCount = 0;
    
    control = code;
    
    while (control != null)
      control.exec(this);
    
    return result;
  }
  
  public Datum popArg()
  {
    Datum x = args.first;
    args = args.rest;
    return x;
  }
  
  public void pushArg(Datum x)
  {
    args = new List(x, args);
  }
}
