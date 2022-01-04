package spartan.errors;

import spartan.parsing.Position;

public class MalformedExpression extends Error
{
  public MalformedExpression(Position position)
  {
    super("malformed expression", position);
  }
}
