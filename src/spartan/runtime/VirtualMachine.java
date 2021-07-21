package spartan.runtime;

import spartan.data.Datum;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.List;
import java.util.ArrayList;
import spartan.errors.RuntimeError;

public class VirtualMachine
{
  public Datum result;
  public Deque<Datum> args = new ArrayDeque<Datum>();
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
}
