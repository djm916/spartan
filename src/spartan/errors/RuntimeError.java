package spartan.errors;

import spartan.parsing.Position;

public class RuntimeError extends SourceError
{
  public RuntimeError(String message, Position position)
  {
    super(message, position);
  }
}
