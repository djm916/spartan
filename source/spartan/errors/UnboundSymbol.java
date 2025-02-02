package spartan.errors;

import spartan.data.Symbol;

public class UnboundSymbol extends Error
{
  private static final String MSG_FMT = "unable to resolve symbol \"%s:%s\"";
  
  public UnboundSymbol(Symbol nsName, Symbol symbol)
  {
    this(nsName, symbol, null);
  }
  
  public UnboundSymbol(Symbol nsName, Symbol symbol, SourceInfo source)
  {
    super(String.format(MSG_FMT, nsName.repr(), symbol.repr()), source);
  }
}
