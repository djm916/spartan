package spartan.errors;

import spartan.data.Symbol;

public class MultipleDefinition extends Error
{
  private static final String MSG_FMT = "multiple definition of symbol \"%s\"";
  
  public MultipleDefinition(Symbol s)
  {
    this(s, null);
  }
  
  public MultipleDefinition(Symbol s, SourceInfo source)
  {
    super(String.format(MSG_FMT, s.repr()), source);
  }
}
