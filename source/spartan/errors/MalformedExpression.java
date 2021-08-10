package spartan.errors;

import spartan.parsing.Position;

public class MalformedExpression extends CompileError
{
  public MalformedExpression(Position position)
  {
    super("malformed expression", position);
  }
}
