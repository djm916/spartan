package spartan.builtins;

import spartan.Namespace;
import spartan.data.Symbol;
import static spartan.builtins.StringLib.*;

public final class StringNS extends Namespace
{
  public StringNS()
  {
    super(Symbol.of("spartan.string"), CoreNS.getInstance());
  }
  
  {
    bind(Symbol.of("concat"), CONCAT);
    bind(Symbol.of("join"), JOIN);
    bind(Symbol.of("substr"), SUBSTR);
    bind(Symbol.of("reverse"), REVERSE);
    bind(Symbol.of("hash"), HASH);
  }
}
