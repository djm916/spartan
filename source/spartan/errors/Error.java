package spartan.errors;

import spartan.errors.SourceInfo;
import spartan.parsing.Position;
import java.util.List;

public class Error extends RuntimeException
{
  public Error(String message)
  {
    super(message);
  }
  
  public Error(String message, SourceInfo source)
  {
    super(message);
    this.source = source;
  }
  
  public Error(String message, SourceInfo source, List<Position> backTrace)
  {
    super(message);
    this.source = source;
    this.backTrace = backTrace;
  }
  
  public SourceInfo getSource()
  {
    return source;
  }
  
  public void setSource(SourceInfo source)
  {
    this.source = source;
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
    var pos = (source == null || source.position() == null) ? "unknown source" : source.position();
    var exp = (source == null || source.exp() == null) ? "none" : source.exp().repr();
    
    message.append(String.format("error: %s: %s in expression: %s", pos, getMessage(), exp));
    
    if (backTrace != null && !backTrace.isEmpty()) {
      message.append("\nbacktrace:");
      for (Position position : backTrace)
        message.append(String.format("\n\t%s", position));
    }
    
    return message.toString();
  }

  private SourceInfo source;
  private List<Position> backTrace;
}
