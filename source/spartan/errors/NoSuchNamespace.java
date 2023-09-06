package spartan.errors;

import spartan.data.Symbol;
import spartan.parsing.Position;

public class NoSuchNamespace extends Error
{
  public NoSuchNamespace(Symbol ns)
  {
    super("no such namespace: \"" + ns.repr() + "\"", null);
  }
  
  public NoSuchNamespace(Symbol ns, Position pos)
  {
    super("no such namespace: \"" + ns.repr() + "\"", pos);
  }
}
