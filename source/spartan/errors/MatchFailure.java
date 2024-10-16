package spartan.errors;

public class MatchFailure extends Error
{
  private static final String MSG_FMT = "pattern match failure";
  
  public MatchFailure()
  {
    this(null);
  }
  
  public MatchFailure(SourceInfo source)
  {
    super(MSG_FMT, source);
  }
}
