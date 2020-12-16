package spartan.errors;

import spartan.Position;

public class RuntimeError extends SourceError
{
  public RuntimeError(String message, Position position)
  {
    super(message, position);
  }
}
