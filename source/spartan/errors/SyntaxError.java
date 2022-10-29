package spartan.errors;

import spartan.parsing.Position;

public class SyntaxError extends Error
{
  public SyntaxError(String message, Position position)
  {
    super("syntax error: " + message, position);
  }
}
