package spartan.errors;

public class InvalidPattern extends Error
{
  private static final String MSG_FMT = "invalid pattern syntax";
  
  public InvalidPattern()
  {
    this(null);
  }
  
  public InvalidPattern(SourceInfo source)
  {
    super(MSG_FMT, source);
  }
}
