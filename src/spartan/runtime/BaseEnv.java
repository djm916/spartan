package spartan.runtime;

import static spartan.builtins.Builtins.*;
import spartan.data.Symbol;

public final class BaseEnv extends GlobalEnv
{
  {
    bind(Symbol.get("+"), Add);
    bind(Symbol.get("-"), Sub);
    bind(Symbol.get("*"), Mul);
    bind(Symbol.get("/"), Div);
    bind(Symbol.get("="), Eq);
    bind(Symbol.get("/="), Ne);
    bind(Symbol.get("<"), Lt);
    bind(Symbol.get(">"), Gt);
    bind(Symbol.get("<="), Le);
    bind(Symbol.get(">="), Ge);
    bind(Symbol.get("~"), Neg);
    
    bind(Symbol.get("car"), Car);
    bind(Symbol.get("cdr"), Cdr);
    bind(Symbol.get("cons"), Cons);
    
    bind(Symbol.get("list"), MakeList);
    bind(Symbol.get("vector"), MakeVector);
    
    //bind(Symbol.get("apply"), Apply);
  }
}
