package spartan.errors;

import spartan.data.Symbol;

public class NoSuchPackage extends Error
{
  private static final String MSG_FMT = "package \"%s\" does not exist";
  
  public NoSuchPackage(Symbol pkg)
  {
    this(pkg, null);
  }
  
  public NoSuchPackage(Symbol pkg, SourceInfo source)
  {
    super(String.format(MSG_FMT, pkg.repr()), source);
  }
}
