package spartan.errors;

import spartan.parsing.Position;
import spartan.data.Symbol;

public class UnboundVariable extends Error
{
  public UnboundVariable(Symbol s)
  {
    super("unbound variable \"" + s.repr());
  }
  
  public UnboundVariable(Symbol s, Position p)
  {
    super("unbound variable \"" + s.repr() + "\"", p);
  }
}
