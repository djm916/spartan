package spartan.errors;

import spartan.data.Symbol;
import spartan.parsing.Position;

public class NoSuchPackage extends Error
{
  private static final String MSG_FMT = "package \"%s\" does not exist";
  
  public NoSuchPackage(Symbol pkg)
  {
    this(pkg, null);
  }
  
  public NoSuchPackage(Symbol pkg, Position pos)
  {
    super(String.format(MSG_FMT, pkg.repr()), pos);
  }
}
