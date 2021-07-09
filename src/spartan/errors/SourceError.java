package spartan.errors;

import spartan.Position;

public class SourceError extends Exception
{
  public final Position position;
  
  public SourceError(String message, Position position)
  {
    super(message);
    this.position = position;
  }
  
  public Throwable fillInStackTrace()
  {
    // Do not have Java generate a stack trace
    return this;
  }
  
  public String toString()
  {
    return String.format(
        "%s: %s: %s: error: %s",
        position.source,
        position.line,
        position.column,
        getMessage());
  }
}
