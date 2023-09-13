package spartan.errors;

import spartan.data.Symbol;
import spartan.parsing.Position;

public class MultipleDefinition extends Error
{
  private static final String MSG_FMT = "multiple definition of symbol \"%s\"";
  
  public MultipleDefinition(Symbol s)
  {
    this(s, null);
  }
  
  public MultipleDefinition(Symbol s, Position pos)
  {
    super(String.format(MSG_FMT, s.repr()), pos);
  }
}
