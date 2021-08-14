package spartan.runtime;

import static spartan.builtins.Core.*;
import static spartan.builtins.Math.*;
import spartan.data.*;

public final class BaseEnv extends GlobalEnv
{
  {
    bind(Symbol.get("true"), Bool.True);
    bind(Symbol.get("false"), Bool.False);
    bind(Symbol.get("nil"), Nil.Instance);
    
    bind(Symbol.get("+"), Add);
    bind(Symbol.get("-"), Sub);
    bind(Symbol.get("*"), Mul);
    bind(Symbol.get("/"), Div);
    bind(Symbol.get("%"), Mod);
    bind(Symbol.get("="), Eq);
    bind(Symbol.get("/="), Ne);
    bind(Symbol.get("<"), Lt);
    bind(Symbol.get(">"), Gt);
    bind(Symbol.get("<="), Le);
    bind(Symbol.get(">="), Ge);
    bind(Symbol.get("~"), Neg);
    bind(Symbol.get("not"), Not);
    bind(Symbol.get("abs"), Abs);
    bind(Symbol.get("floor"), Floor);
    bind(Symbol.get("ceil"), Ceil);
    bind(Symbol.get("exp"), Exp);
    bind(Symbol.get("log"), Log);
    bind(Symbol.get("sin"), Sin);
    bind(Symbol.get("cos"), Cos);
    bind(Symbol.get("tan"), Tan);
    bind(Symbol.get("asin"), Sin);
    bind(Symbol.get("acos"), Cos);
    bind(Symbol.get("atan"), Tan);
    
    bind(Symbol.get("E"), Real.E);
    bind(Symbol.get("PI"), Real.PI);
    
    bind(Symbol.get("car"), Car);
    bind(Symbol.get("cdr"), Cdr);
    bind(Symbol.get("cons"), Cons);
    
    bind(Symbol.get("list"), MakeList);
    bind(Symbol.get("vector"), MakeVector);
    bind(Symbol.get("record"), MakeRecord);
    
    bind(Symbol.get("apply"), Apply);
  }
}
