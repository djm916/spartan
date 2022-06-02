package spartan.compiling;

import java.util.List;
import java.util.LinkedList;
import spartan.runtime.Inst;
import spartan.runtime.Jump;

class RecurPoint
{
  RecurPoint(int numBindings)
  {
    this.numBindings = numBindings;
    this.jumps = new LinkedList<>();
  }

  int numBindings()
  {
    return numBindings;
  }
  
  void addJump(Jump jump)
  {
    jumps.add(jump);
  }
  
  void setJumpTargets(Inst target)
  {
    for (var jump : jumps)
      jump.setTarget(target);
  }
  
  private final int numBindings;
  private final List<Jump> jumps;
}
