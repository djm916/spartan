package spartan.errors;

import spartan.parsing.Position;

public class CompileError extends SourceError
{
  public CompileError(String message, Position position)
  {
    super(message, position);
  }
}
