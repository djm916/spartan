package spartan.errors;

import spartan.data.Symbol;

public class NoSuchNamespace extends Error
{
  private static final String MSG_FMT = "namespace \"%s\" does not exist";
  
  public NoSuchNamespace(Symbol nsName)
  {
    this(nsName, null);
  }
  
  public NoSuchNamespace(Symbol nsName, SourceInfo source)
  {
    super(String.format(MSG_FMT, nsName.repr()), source);
  }
}
