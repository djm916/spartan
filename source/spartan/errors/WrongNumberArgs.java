package spartan.errors;

import spartan.parsing.Position;

public class WrongNumberArgs extends Error
{
  private static final String MSG_FMT = "incorrect number of arguments";
  
  public WrongNumberArgs()
  {
    this(null);
  }
  
  public WrongNumberArgs(Position pos)
  {
    super(MSG_FMT, pos);
  }
}
