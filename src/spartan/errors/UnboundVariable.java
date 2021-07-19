package spartan.errors;

import spartan.parsing.Position;
import spartan.data.Symbol;

public class UnboundVariable extends RuntimeError
{
  public UnboundVariable(Symbol s, Position p)
  {
    super("unbound variable \"" + s.repr() + "\"", p);
  }
}
