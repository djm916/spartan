package spartan.runtime;

import spartan.data.List;
import spartan.parsing.Position;

final class Frame
{
  final Frame parent;
  final LocalEnv locals;
  final List args;
  final Inst returnTo;
  final Position position;
  
  Frame(Frame parent, LocalEnv locals, List args, Inst returnTo, Position position)
  {
    this.parent = parent;
    this.locals = locals;
    this.args = args;
    this.returnTo = returnTo;
    this.position = position;
  }
}
