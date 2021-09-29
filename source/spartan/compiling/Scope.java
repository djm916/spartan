package spartan.compiling;

import spartan.data.List;
import spartan.data.Symbol;

class Scope
{
  private final Scope parent;
  private final List bindings;
  
  Scope(Scope parent)
  {
    this.parent = parent;
    this.bindings = List.Empty;
  }
  
  Scope(Scope parent, List bindings)
  {
    this.parent = parent;
    this.bindings = bindings;
  }
  
  DeBruijnIndex lookup(Symbol s)
  {
    return lookup(s, 0);
  }
  
  Scope bind(Symbol s)
  {
    return new Scope(parent, bindings.append(s));
  }
  
  private int offsetOf(Symbol s)
  {
    int i = 0;
    for (List bindings = this.bindings; !bindings.empty(); bindings = bindings.cdr(), ++i)
      if (bindings.car() == s)
        return i;
    return -1;
  }
  
  private DeBruijnIndex lookup(Symbol s, int depth)
  {
    int offset = offsetOf(s);
    if (offset >= 0)
      return new DeBruijnIndex(depth, offset);
    else if (parent != null)
      return parent.lookup(s, depth + 1);
    else
      return null;
  }
}
