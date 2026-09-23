package spartan.errors;

public class InexactArgument extends Error
{
  private static final String MSG_FMT = "inexact argument(s) given where exact argument(s) required";
  
  public InexactArgument()
  {
    super(MSG_FMT);
  }
}
