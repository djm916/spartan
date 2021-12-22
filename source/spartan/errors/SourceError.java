package spartan.errors;

import spartan.parsing.Position;

public class SourceError extends Exception
{
  public final Position position;
  
  public SourceError(String message, Position position)
  {
    super(message);
    this.position = position;
  }
  
  // Do not have Java generate a stack trace
  public Throwable fillInStackTrace()
  {
    return this;
  }
}
