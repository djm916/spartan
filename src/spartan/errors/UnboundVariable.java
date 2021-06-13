package spartan.errors;

import spartan.Position;

public class UnboundVariable extends RuntimeError
{
  public UnboundVariable(String id, Position position)
  {
    super("unbound variable \"" + id + "\"", position);
  }
}
