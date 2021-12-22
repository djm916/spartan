package spartan.errors;

import spartan.parsing.Position;

public class RuntimeError extends SourceError
{
  public RuntimeError(String message, Position position)
  {
    super(message, position);
  }
  
  public void setBackTrace(java.util.List<Position> backTrace)
  {
    this.backTrace = backTrace;
  }
  
  public String toString()
  {
    var message = new StringBuilder();
    
    if (position == null)
      message.append(String.format("error: unknown source: %s\n", getMessage()));
    else
      message.append(String.format("error: %s: %s\n", position, getMessage()));
    
    for (Position position : backTrace)
      message.append(String.format("\tfrom %s\n", position));
    
    return message.toString();
  }
    
  private java.util.List<Position> backTrace;
}
