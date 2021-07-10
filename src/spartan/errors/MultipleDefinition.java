package spartan.errors;

import spartan.data.Symbol;

public class MultipleDefinition extends Error
{
  public MultipleDefinition(Symbol s)
  {
    super("multiple definition of \"" + s.repr() + "\"");
  }
}
