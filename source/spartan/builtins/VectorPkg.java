package spartan.builtins;

import spartan.Package;
import spartan.data.Symbol;
import static spartan.builtins.VectorLib.*;

public final class VectorPkg extends Package
{
  public VectorPkg()
  {
    super(Symbol.of("vector"));
  }
  
  {
    bind(Symbol.of("vector"), FROM_LIST);
    bind(Symbol.of("new"), NEW);
    bind(Symbol.of("copy"), COPY);
    bind(Symbol.of("append!"), APPEND);
    bind(Symbol.of("insert!"), INSERT);
    bind(Symbol.of("remove!"), REMOVE);
  }
}
