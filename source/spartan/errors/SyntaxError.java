package spartan.errors;

import spartan.parsing.Position;

public class SyntaxError extends SourceError
{
  public SyntaxError(String message, Position position)
  {
    super(message, position);
  }
}
