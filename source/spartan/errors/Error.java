package spartan.errors;

import spartan.parsing.Position;
import java.util.List;

public class Error extends RuntimeException
{
  public Error(String message)
  {
    super(message);
  }
  
  public Error(String message, Position position)
  {
    super(message);
    this.position = position;
  }
  
  public Position getPosition()
  {
    return position;
  }
  
  public void setPosition(Position position)
  {
    this.position = position;
  }
  
  public List<Position> getBackTrace()
  {
    return backTrace;
  }
  
  public void setBackTrace(List<Position> backTrace)
  {
    this.backTrace = backTrace;
  }
  
  // Do not generate Java stack trace
  public Throwable fillInStackTrace()
  {
    return this;
  }
  
  public String toString()
  {
    var message = new StringBuilder();
    
    message.append(String.format("error: %s: %s", (position == null ? "unknown source" : position), getMessage()));
    
    if (backTrace != null && !backTrace.isEmpty()) {
      message.append("\nbacktrace:");
      for (Position position : backTrace)
        message.append(String.format("\n\t%s", position));
    }
    
    return message.toString();
  }

  private Position position;
  private List<Position> backTrace;
}
