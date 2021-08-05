package spartan.runtime;

import spartan.data.List;

public final class Frame
{
  public final Frame parent;
  public final LocalEnv locals;
  public final List args;
  public final Inst returnTo;
  
  Frame(Frame parent, LocalEnv locals, List args, Inst returnTo)
  {
    this.parent = parent;
    this.locals = locals;
    this.args = args;
    this.returnTo = returnTo;
  }
}
