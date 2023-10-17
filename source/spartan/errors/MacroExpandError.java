package spartan.errors;

import spartan.parsing.Position;

public class MacroExpandError extends Error
{
  private static final String MSG_FMT = "in expansion of macro \"%s\": %s";
  
  public MacroExpandError(String macroName, Position position, Error cause)
  {
    super(String.format(MSG_FMT, macroName, cause.getMessage()), position);
  }
}
