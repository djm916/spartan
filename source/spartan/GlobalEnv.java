package spartan;

import spartan.data.*;
import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.TextLib;
import spartan.builtins.PortLib;
import java.util.Map;
import java.util.HashMap;

public final class GlobalEnv
{
  public static GlobalEnv createBasis()
  {
    return new GlobalEnv();
  }
  
  public void bind(Symbol name, Datum val)
  {
    globals.put(name, val);
  }
  
  public Datum lookup(Symbol name)
  {
    return globals.get(name);
  }
  
  private GlobalEnv() {}
  
  private final Map<Symbol, Datum> globals = new HashMap<>();
  
  {
    /* General procedures */
    
    bind(new Symbol("apply"), CoreLib.Apply);    
    bind(new Symbol("print"), CoreLib.Print);
    bind(new Symbol("print-line"), CoreLib.PrintLine);
    bind(new Symbol("type"), CoreLib.TypeOf);
    bind(new Symbol("length"), CoreLib.Length);
    bind(new Symbol("load"), CoreLib.Load);
    bind(new Symbol("="), CoreLib.Eq);
    bind(new Symbol("/="), CoreLib.Ne);
    bind(new Symbol("<"), CoreLib.Lt);
    bind(new Symbol(">"), CoreLib.Gt);
    bind(new Symbol("<="), CoreLib.Le);
    bind(new Symbol(">="), CoreLib.Ge);
    bind(new Symbol("not"), CoreLib.Not);    
    bind(new Symbol("gensym"), CoreLib.GenSym);
    bind(new Symbol("identity-hash"), CoreLib.IdentityHash);
    bind(new Symbol("error"), CoreLib.Error);    
    bind(new Symbol("format-decimal"), CoreLib.FormatDecimal);
    
    /* Type predicates */
    
    bind(new Symbol("nil?"), CoreLib.IsNil);
    bind(new Symbol("empty?"), CoreLib.IsEmptyList);
    bind(new Symbol("boolean?"), CoreLib.IsBoolean);
    bind(new Symbol("integer?"), CoreLib.IsInteger);
    bind(new Symbol("real?"), CoreLib.IsReal);
    bind(new Symbol("complex?"), CoreLib.IsComplex);
    bind(new Symbol("number?"), CoreLib.IsNumber);
    bind(new Symbol("symbol?"), CoreLib.IsSymbol);
    bind(new Symbol("text?"), CoreLib.IsText);
    //bind(new Symbol("bytes?"), CoreLib.IsBytes);
    //bind(new Symbol("list?"), CoreLib.IsList);
    //bind(new Symbol("vector?"), CoreLib.IsVector);
    bind(new Symbol("callable?"), CoreLib.IsCallable);
    
    /* Math constants & procedures */
    
    bind(new Symbol("complex"), MathLib.MakeComplex);
    bind(new Symbol("E"), Real.E);
    bind(new Symbol("PI"), Real.Pi);
    bind(new Symbol("+inf"), Real.PositiveInfinity);
    bind(new Symbol("-inf"), Real.NegativeInfinity);
    bind(new Symbol("NaN"), Real.NotANumber);
    bind(new Symbol("I"), Complex.I);    
    bind(new Symbol("+"), MathLib.Add);
    bind(new Symbol("-"), MathLib.Sub);
    bind(new Symbol("*"), MathLib.Mul);
    bind(new Symbol("/"), MathLib.Div);
    bind(new Symbol("~"), MathLib.Neg);
    bind(new Symbol("abs"), MathLib.Abs);
    bind(new Symbol("floor"), MathLib.Floor);
    bind(new Symbol("ceiling"), MathLib.Ceiling);
    bind(new Symbol("round"), MathLib.Round);
    bind(new Symbol("truncate"), MathLib.Truncate);
    bind(new Symbol("quotient"), MathLib.Quotient);
    bind(new Symbol("remainder"), MathLib.Remainder);
    bind(new Symbol("exp"), MathLib.Exp);
    bind(new Symbol("log"), MathLib.Log);
    bind(new Symbol("sin"), MathLib.Sin);
    bind(new Symbol("cos"), MathLib.Cos);
    bind(new Symbol("tan"), MathLib.Tan);
    bind(new Symbol("asin"), MathLib.Sin);
    bind(new Symbol("acos"), MathLib.Cos);
    bind(new Symbol("atan"), MathLib.Tan);    
    bind(new Symbol("rand"), MathLib.Rand);
    bind(new Symbol("real-part"), MathLib.RealPart);
    bind(new Symbol("imag-part"), MathLib.ImagPart);
    bind(new Symbol("rect->polar"), MathLib.RectToPolar);
    bind(new Symbol("polar->rect"), MathLib.PolarToRect);
    
    /* Conversion procedures */
    
    bind(new Symbol("text->symbol"), CoreLib.TextToSymbol);
    bind(new Symbol("symbol->text"), CoreLib.SymbolToText);    
    bind(new Symbol("text->bytes"), CoreLib.TextToBytes); // encode
    bind(new Symbol("bytes->text"), CoreLib.BytesToText); // decode
    
    /* List procedures */

    bind(new Symbol("list"), ListLib.MakeList);
    bind(new Symbol("cons"), ListLib.Cons);
    bind(new Symbol("car"), ListLib.Car);
    bind(new Symbol("caar"), ListLib.Caar);
    bind(new Symbol("cadr"), ListLib.Cadr);
    bind(new Symbol("caddr"), ListLib.Caddr);
    bind(new Symbol("cdr"), ListLib.Cdr);
    bind(new Symbol("cddr"), ListLib.Cddr);
    bind(new Symbol("cdddr"), ListLib.Cdddr);
    bind(new Symbol("concat"), ListLib.Concat);
    bind(new Symbol("append"), ListLib.Append);
    bind(new Symbol("reverse"), ListLib.Reverse);
    bind(new Symbol("set-car!"), ListLib.SetCar);
    bind(new Symbol("set-cdr!"), ListLib.SetCdr);
    
    /* Vector procedures */
    
    bind(new Symbol("vector"), VectorLib.MakeVector);
    bind(new Symbol("vector/new"), VectorLib.New);
    bind(new Symbol("vector/length"), VectorLib.Length);
    bind(new Symbol("vector/copy"), VectorLib.Copy);
    bind(new Symbol("vector/ref"), VectorLib.Ref);
    bind(new Symbol("vector/set!"), VectorLib.Set);
    bind(new Symbol("vector/append!"), VectorLib.Append);
       
    /* Text procedures */
    
    bind(new Symbol("text/concat"), TextLib.Concat);
    bind(new Symbol("text/hash"), TextLib.Hash);
        
    /* Port procedures */
    
    bind(new Symbol("port/open"), PortLib.Open);
    bind(new Symbol("port/close"), PortLib.Close);
    bind(new Symbol("port/read"), PortLib.Read);
    bind(new Symbol("port/write"), PortLib.Write);
    bind(new Symbol("port/stdin"), InputPort.Stdin);
    bind(new Symbol("port/stdout"), OutputPort.Stdout);
  }
}
