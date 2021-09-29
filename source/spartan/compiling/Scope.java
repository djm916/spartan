package spartan.compiling;

import spartan.data.List;
import spartan.data.Symbol;

class Scope
{
  private final Scope parent;
  private final List symbols;
  
  Scope(Scope parent)
  {
    this.parent = parent;
    this.symbols = List.Empty;
  }
  
  Scope(Scope parent, List symbols)
  {
    this.parent = parent;
    this.symbols = symbols;
  }
  
  DeBruijnIndex lookup(Symbol s)
  {
    return lookup(s, 0);
  }
  
  Scope bind(Symbol s)
  {
    return new Scope(parent, symbols.append(s));
  }
    
  private int offsetOf(Symbol s)
  {
    int offset = 0;
    for (List symbols = this.symbols; !symbols.empty(); symbols = symbols.cdr(), ++offset)
      if (symbols.car() == s)
        return offset;
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
