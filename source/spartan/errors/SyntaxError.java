package spartan.errors;

import spartan.parsing.Position;

public class SyntaxError extends Error
{
  private static final String MSG_FMT = "syntax error: %s";
  
  public SyntaxError(String message, Position position)
  {
    super(String.format(MSG_FMT, message), position);
  }
}
