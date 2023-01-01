package spartan.errors;

import spartan.parsing.Position;

public class WrongNumberArgs extends Error
{
  public WrongNumberArgs()
  {
    super("incorrect number of arguments");
  }
  
  public WrongNumberArgs(Position pos)
  {
    super("incorrect number of arguments", pos);
  }
}
