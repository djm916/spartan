package spartan.builtins;

import spartan.NameSpace;
import spartan.data.Symbol;
import static spartan.builtins.VectorLib.*;

public final class VectorNS extends NameSpace
{
  public VectorNS(NameSpace parent)
  {
    super(Symbol.of("vector"), parent);
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
