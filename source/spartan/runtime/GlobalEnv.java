package spartan.runtime;

import spartan.data.*;
import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.TextLib;
import java.util.Map;
import java.util.IdentityHashMap;

public final class GlobalEnv
{
  {
    globals = new IdentityHashMap<>();
    
    /* General procedures */
    
    bind(Symbol.get("list"), CoreLib.MakeList);
    bind(Symbol.get("vector"), CoreLib.MakeVector);
    bind(Symbol.get("complex"), CoreLib.MakeComplex);
    bind(Symbol.get("ratio"), CoreLib.MakeRatio);
    bind(Symbol.get("apply"), CoreLib.Apply);    
    bind(Symbol.get("print"), CoreLib.Print);
    bind(Symbol.get("print-line"), CoreLib.PrintLine);
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
    bind(Symbol.get("gensym"), CoreLib.GenSym);
    bind(Symbol.get("identity-hash"), CoreLib.IdentityHash);
    
    /* Type predicates */
    
    bind(Symbol.get("nil?"), CoreLib.IsNil);
    bind(Symbol.get("empty?"), CoreLib.IsEmptyList);
    
    /* Math procedures */
    
    bind(Symbol.get("E"), Real.E);
    bind(Symbol.get("PI"), Real.PI);
    bind(Symbol.get("I"), Complex.I);
    bind(Symbol.get("+"), MathLib.Add);
    bind(Symbol.get("-"), MathLib.Sub);
    bind(Symbol.get("*"), MathLib.Mul);
    bind(Symbol.get("/"), MathLib.Div);
    bind(Symbol.get("%"), MathLib.Mod);    
    bind(Symbol.get("~"), MathLib.Neg);
    bind(Symbol.get("abs"), MathLib.Abs);
    bind(Symbol.get("floor"), MathLib.Floor);
    bind(Symbol.get("ceiling"), MathLib.Ceiling);
    bind(Symbol.get("exp"), MathLib.Exp);
    bind(Symbol.get("log"), MathLib.Log);
    bind(Symbol.get("sin"), MathLib.Sin);
    bind(Symbol.get("cos"), MathLib.Cos);
    bind(Symbol.get("tan"), MathLib.Tan);
    bind(Symbol.get("asin"), MathLib.Sin);
    bind(Symbol.get("acos"), MathLib.Cos);
    bind(Symbol.get("atan"), MathLib.Tan);    
    bind(Symbol.get("rand"), MathLib.Rand);
    bind(Symbol.get("numerator"), MathLib.Numerator);
    bind(Symbol.get("denominator"), MathLib.Denominator);
    bind(Symbol.get("real-part"), MathLib.RealPart);
    bind(Symbol.get("imag-part"), MathLib.ImagPart);
    
    /* Conversion procedures */
    
    bind(Symbol.get("text->symbol"), CoreLib.TextToSymbol);
    bind(Symbol.get("symbol->text"), CoreLib.SymbolToText);
    bind(Symbol.get("int->real"), CoreLib.IntToReal);   
    bind(Symbol.get("real->int"), CoreLib.RealToInt);
        
    /* List procedures */
    
    bind(Symbol.get("cons"), ListLib.Cons);
    bind(Symbol.get("car"), ListLib.Car);
    bind(Symbol.get("caar"), ListLib.Caar);
    bind(Symbol.get("cadr"), ListLib.Cadr);
    bind(Symbol.get("caddr"), ListLib.Caddr);
    bind(Symbol.get("cdr"), ListLib.Cdr);
    bind(Symbol.get("cddr"), ListLib.Cddr);
    bind(Symbol.get("cdddr"), ListLib.Cdddr);
    bind(Symbol.get("concat"), ListLib.Concat);
    bind(Symbol.get("append"), ListLib.Append);
    bind(Symbol.get("reverse"), ListLib.Reverse);
    bind(Symbol.get("set-car!"), ListLib.SetCar);
    bind(Symbol.get("set-cdr!"), ListLib.SetCdr);
    
    /* Vector procedures */
    
    bind(Symbol.get("vector/new"), VectorLib.New);
    bind(Symbol.get("vector/copy"), VectorLib.Copy);
    bind(Symbol.get("vector/get"), VectorLib.Get);
    bind(Symbol.get("vector/set!"), VectorLib.Set);
    bind(Symbol.get("vector/append!"), VectorLib.Append);
       
    /* Text procedures */
    
    bind(Symbol.get("text/concat"), TextLib.Concat);
    bind(Symbol.get("text/hash"), TextLib.Hash);
    bind(Symbol.get("text/format"), TextLib.Format);
  }
  
  public static GlobalEnv createBasis()
  {
    return new GlobalEnv();
  }
  
  public void bind(Symbol s, Datum x)
  {
    globals.put(s, x);
  }
  
  public Datum lookup(Symbol s)
  {
    return globals.getOrDefault(s, Nil.Value);
  }
  
  private GlobalEnv() {}
  
  private final Map<Symbol, Datum> globals;
}
