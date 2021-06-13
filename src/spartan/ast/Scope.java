package spartan.ast;

import java.util.Optional;

class Scope
{
  public final String id;
  public final Scope parent;
  
  Scope(String id, Scope parent)
  {
    this.id = id;
    this.parent = parent;
  }
  
  Integer lookup(String id)
  {
    Scope scope = this;
    int depth = 0;
    for (; scope != null; scope = scope.parent, ++depth) {
      if (scope.id.equals(id))
        return depth;
    }
    return null;
  }
}
