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
    
    message.append(super.toString());    
    
    for (Position position : backTrace)
      message.append(String.format("\n\tfrom %s", position));
    
    return message.toString();
  }
    
  private java.util.List<Position> backTrace;
}
