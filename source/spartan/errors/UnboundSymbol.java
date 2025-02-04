package spartan.errors;

import spartan.data.Symbol;

public class UnboundSymbol extends Error
{
  private static final String MSG_FMT = "unable to resolve symbol \"%s\"";
  
  public UnboundSymbol(Symbol symbol)
  {
    this(symbol, null);
  }
  
  public UnboundSymbol(Symbol symbol, SourceInfo source)
  {
    super(String.format(MSG_FMT, symbol.repr()), source);
  }
}
