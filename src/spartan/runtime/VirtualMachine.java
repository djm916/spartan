package spartan.runtime;

import spartan.data.Value;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.List;
import java.util.ArrayList;
import spartan.errors.RuntimeError;

public class VirtualMachine
{
  public Value result;
  public Deque<Value> args = new ArrayDeque<Value>();
  public Inst control;
  public LocalEnv locals;
  public GlobalEnv globals;
  public Frame frame;
  
  public VirtualMachine(GlobalEnv globals)
  {
    this.globals = globals;
  }
  
  public Value exec(Inst code) throws RuntimeError
  {
    control = code;
    
    while (control != null)
      control.exec(this);
    
    return result;
  }
}
