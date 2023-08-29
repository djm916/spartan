package spartan.builtins;

import spartan.Package;
import spartan.data.Symbol;
import static spartan.builtins.StringLib.*;

public final class StringPkg extends Package
{
  public StringPkg()
  {
    super(Symbol.of("string"));
  }
  
  {
    bind(Symbol.of("concat"), CONCAT);
    bind(Symbol.of("join"), JOIN);
    bind(Symbol.of("substr"), SUBSTR);
    bind(Symbol.of("reverse"), REVERSE);
    bind(Symbol.of("hash"), HASH);
  }
}
