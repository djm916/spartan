package spartan.builtins;

import spartan.NameSpace;
import spartan.data.Symbol;
import static spartan.builtins.StringLib.*;

public final class StringNS extends NameSpace
{
  public StringNS(NameSpace parent)
  {
    super(Symbol.of("string"), parent);
  }
  
  {
    bind(Symbol.of("concat"), CONCAT);
    bind(Symbol.of("join"), JOIN);
    bind(Symbol.of("substr"), SUBSTR);
    bind(Symbol.of("reverse"), REVERSE);
    bind(Symbol.of("hash"), HASH);
  }
}
