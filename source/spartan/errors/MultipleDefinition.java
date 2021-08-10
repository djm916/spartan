package spartan.errors;

import spartan.data.Symbol;
import spartan.parsing.Position;

public class MultipleDefinition extends CompileError
{
  public MultipleDefinition(Symbol s, Position p)
  {
    super("multiple definition of \"" + s.repr() + "\"", p);
  }
}
