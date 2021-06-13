package spartan.runtime;

import spartan.data.Value;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.List;
import java.util.ArrayList;
import spartan.errors.RuntimeError;

public class VirtualMachine
{
  Value result;
  Deque<Value> args = new ArrayDeque<Value>();
  Inst control;
  LocalEnv locals;
  GlobalEnv globals = new GlobalEnv();
  Frame frame;
  
  public Value exec(Inst code) throws RuntimeError
  {
    control = code;
    
    while (control != null)
      control.exec(this);
    
    return result;
  }
}
