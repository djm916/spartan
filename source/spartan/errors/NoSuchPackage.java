package spartan.errors;

import spartan.data.Symbol;
import spartan.parsing.Position;

public class NoSuchPackage extends Error
{
  public NoSuchPackage(Symbol ns)
  {
    super("no such package: \"" + ns.repr() + "\"", null);
  }
  
  public NoSuchPackage(Symbol ns, Position pos)
  {
    super("no such package: \"" + ns.repr() + "\"", pos);
  }
}
