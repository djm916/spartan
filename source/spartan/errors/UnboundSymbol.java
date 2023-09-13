package spartan.errors;

import spartan.parsing.Position;
import spartan.data.Symbol;

public class UnboundSymbol extends Error
{
  private static final String MSG_FMT = "unable to resolve symbol \"%s:%s\"";
  
  public UnboundSymbol(Symbol pkg, Symbol sym)
  {
    this(pkg, sym, null);
  }
  
  public UnboundSymbol(Symbol pkg, Symbol sym, Position pos)
  {
    super(String.format(MSG_FMT, pkg.repr(), sym.repr()), pos);
  }
}
