package spartan.builtins;

import spartan.Namespace;
import spartan.data.Symbol;
import static spartan.builtins.TableLib.*;

public final class TableNS extends Namespace
{
  public TableNS()
  {
    super(Symbol.of("spartan.table"), CoreNS.getInstance());
  }
  
  {
    bind(Symbol.of("table"), FROM_LIST);
    bind(Symbol.of("contains?"), CONTAINS);
    bind(Symbol.of("keys"), KEYS);
    bind(Symbol.of("values"), VALUES);
  }
}
