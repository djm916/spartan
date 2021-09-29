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
  
  Scope bind(Symbol s)
  {
    return new Scope(parent, symbols.append(s));
  }
  
  DeBruijnIndex lookup(Symbol s)
  {
    int depth = 0;
    var scope = this;    
    for (; scope != null; ++depth) {
      int offset = scope.offsetOf(s);
      if (offset >= 0)
        return new DeBruijnIndex(depth, offset);
      scope = scope.parent;
    }
    return null;
  }
  
  private int offsetOf(Symbol s)
  {
    int offset = 0;
    var symbols = this.symbols;
    for (; !symbols.empty(); symbols = symbols.cdr(), ++offset)
      if (symbols.car() == s)
        return offset;
    return -1;
  }
}
