package spartan.errors;

public class WrongNumberArgs extends Error
{
  private static final String MSG_FMT = "incorrect number of arguments";
  
  public WrongNumberArgs()
  {
    this(null);
  }
  
  public WrongNumberArgs(SourceInfo source)
  {
    super(MSG_FMT, source);
  }
}
