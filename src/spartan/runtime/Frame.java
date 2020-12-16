package spartan.runtime;

public class Frame
{
  public final Frame parent;
  public final LocalEnv locals;
  public final Inst returnTo;
  
  public Frame(Frame parent, LocalEnv locals, Inst returnTo)
  {
    this.parent = parent;
    this.locals = locals;
    this.returnTo = returnTo;
  }
}
