package spartan.errors;

import spartan.Position;

public class CompileError extends SourceError
{
  public CompileError(String message, Position position)
  {
    super(message, position);
  }
}
