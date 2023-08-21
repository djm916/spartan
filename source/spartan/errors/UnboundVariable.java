package spartan.errors;

import spartan.parsing.Position;
import spartan.data.Symbol;

public class UnboundVariable extends Error
{  
  public UnboundVariable(Symbol ns, Symbol s, Position p)
  {
    super("unbound variable \"" + ns.repr() + ":" + s.repr() + "\"", p);
  }
}
