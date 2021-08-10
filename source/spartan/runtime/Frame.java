package spartan.runtime;

import spartan.data.List;

final class Frame
{
  final Frame parent;
  final LocalEnv locals;
  final List args;
  final Inst returnTo;
  
  Frame(Frame parent, LocalEnv locals, List args, Inst returnTo)
  {
    this.parent = parent;
    this.locals = locals;
    this.args = args;
    this.returnTo = returnTo;
  }
}
