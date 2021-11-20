package spartan.runtime;

import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.TextLib;
import spartan.data.*;

public final class BaseEnv extends GlobalEnv
{
  {
    bind(Symbol.get("true"), Bool.True);
    bind(Symbol.get("false"), Bool.False);
    bind(Symbol.get("nil"), Nil.Instance);
    
    bind(Symbol.get("list"), CoreLib.MakeList);
    bind(Symbol.get("vector"), CoreLib.MakeVector);
    bind(Symbol.get("record"), CoreLib.MakeRecord);
    bind(Symbol.get("complex"), CoreLib.MakeComplex);    
    bind(Symbol.get("apply"), CoreLib.Apply);    
    bind(Symbol.get("print"), CoreLib.Print);
    bind(Symbol.get("printnl"), CoreLib.PrintLine);
    bind(Symbol.get("type"), CoreLib.TypeOf);
    bind(Symbol.get("length"), CoreLib.Length);
    bind(Symbol.get("load"), CoreLib.Load);
    bind(Symbol.get("="), CoreLib.Eq);
    bind(Symbol.get("/="), CoreLib.Ne);
    bind(Symbol.get("<"), CoreLib.Lt);
    bind(Symbol.get(">"), CoreLib.Gt);
    bind(Symbol.get("<="), CoreLib.Le);
    bind(Symbol.get(">="), CoreLib.Ge);
    bind(Symbol.get("not"), CoreLib.Not);
    bind(Symbol.get("text->symbol"), CoreLib.TextToSymbol);
    bind(Symbol.get("symbol->text"), CoreLib.SymbolToText);
    bind(Symbol.get("gensym"), CoreLib.GenSym);
    
    bind(Symbol.get("+"), MathLib.Add);
    bind(Symbol.get("-"), MathLib.Sub);
    bind(Symbol.get("*"), MathLib.Mul);
    bind(Symbol.get("/"), MathLib.Div);
    bind(Symbol.get("%"), MathLib.Mod);
    
    bind(Symbol.get("~"), MathLib.Neg);
    bind(Symbol.get("abs"), MathLib.Abs);
    bind(Symbol.get("floor"), MathLib.Floor);
    bind(Symbol.get("ceil"), MathLib.Ceil);
    bind(Symbol.get("exp"), MathLib.Exp);
    bind(Symbol.get("log"), MathLib.Log);
    bind(Symbol.get("sin"), MathLib.Sin);
    bind(Symbol.get("cos"), MathLib.Cos);
    bind(Symbol.get("tan"), MathLib.Tan);
    bind(Symbol.get("asin"), MathLib.Sin);
    bind(Symbol.get("acos"), MathLib.Cos);
    bind(Symbol.get("atan"), MathLib.Tan);    
    bind(Symbol.get("E"), Real.E);
    bind(Symbol.get("PI"), Real.PI);
    bind(Symbol.get("I"), Complex.I);
    bind(Symbol.get("math/rand"), MathLib.Rand);
    
    bind(Symbol.get("empty?"), ListLib.IsEmpty);
    bind(Symbol.get("cons"), ListLib.Cons);
    bind(Symbol.get("car"), ListLib.Car);
    bind(Symbol.get("cadr"), ListLib.Cadr);
    bind(Symbol.get("caddr"), ListLib.Caddr);
    bind(Symbol.get("cdr"), ListLib.Cdr);
    bind(Symbol.get("cddr"), ListLib.Cddr);
    bind(Symbol.get("cdddr"), ListLib.Cdddr);
    bind(Symbol.get("list/concat"), ListLib.Concat);
    bind(Symbol.get("list/append"), ListLib.Append);
    
    bind(Symbol.get("vector/new"), VectorLib.New);
    bind(Symbol.get("vector/copy"), VectorLib.Copy);
    bind(Symbol.get("vector/get"), VectorLib.Get);
    bind(Symbol.get("vector/set!"), VectorLib.Set);
    
    bind(Symbol.get("text/concat"), TextLib.Concat);    
  }
}
