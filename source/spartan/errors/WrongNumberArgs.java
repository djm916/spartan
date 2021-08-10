package spartan.errors;

public class WrongNumberArgs extends Error
{
  public WrongNumberArgs()
  {
    super("incorrect number of arguments");
  }
}
