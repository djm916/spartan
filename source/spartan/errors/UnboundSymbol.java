package spartan.errors;

import spartan.parsing.Position;
import spartan.data.Symbol;

public class UnboundSymbol extends Error
{
  public UnboundSymbol(Symbol pkg, Symbol s)
  {
    super("unbound symbol \"" + pkg.repr() + ":" + s.repr());
  }
  
  public UnboundSymbol(Symbol pkg, Symbol s, Position p)
  {
    super("unbound symbol \"" + pkg.repr() + ":" + s.repr() + "\"", p);
  }
}
