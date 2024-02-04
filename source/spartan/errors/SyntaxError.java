package spartan.errors;

public class SyntaxError extends Error
{
  private static final String MSG_FMT = "syntax error: %s";
  
  public SyntaxError(String message, SourceInfo source)
  {
    super(String.format(MSG_FMT, message), source);
  }
}
