package spartan;

import spartan.data.*;
import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.TextLib;
import spartan.builtins.PortLib;
import spartan.errors.UnboundVariable;
import java.util.Map;
import java.util.HashMap;
import java.util.function.Function;
import java.util.function.Supplier;

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
  
  /** Lookup a variable in the global environment.
      @param name The variable to look up
      @return The value of the variable
      @throws UnboundVariable if the variable is not found
  */
  public Datum lookup(Symbol name)
  {
    var value = globals.get(name);
    if (value == null)
      throw new UnboundVariable(name);
    return value;
  }
  
  /** Lookup a variable in the global environment.
      If the variable is found, returns the value of invoking ifPresent on the
      variable's value. Otherwise, returns the value of invoking getAbsent.
      @param name The variable to look up
      @param ifPresent The function to invoke if the variable was found
      @param ifAbsent The supplier to invoke if the variable was not found
      @return The value returned by ifPresent or ifAbsent
  */
  public <R> R lookupOrElse(Symbol name, Function<Datum, R> ifPresent, Supplier<R> ifAbsent)
  {
    if (globals.containsKey(name))
      return ifPresent.apply(globals.get(name));
    else
      return ifAbsent.get();
  }
  
  private GlobalEnv() {}
  
  private final Map<Symbol, Datum> globals = new HashMap<>();
  
  {
    /* General values & procedures */
    
    bind(new Symbol("nil"), Nil.VALUE);
    bind(new Symbol("true"), Bool.TRUE);
    bind(new Symbol("false"), Bool.FALSE);    
    bind(new Symbol("apply"), CoreLib.APPLY);
    bind(new Symbol("print"), CoreLib.PRINT);
    bind(new Symbol("print-line"), CoreLib.PRINT_LINE);
    bind(new Symbol("type"), CoreLib.TYPE);
    bind(new Symbol("length"), CoreLib.LENGTH);
    bind(new Symbol("load"), CoreLib.LOAD);
    bind(new Symbol("="), CoreLib.EQ);
    bind(new Symbol("/="), CoreLib.NE);
    bind(new Symbol("<"), CoreLib.LT);
    bind(new Symbol(">"), CoreLib.GT);
    bind(new Symbol("<="), CoreLib.LE);
    bind(new Symbol(">="), CoreLib.GE);
    bind(new Symbol("not"), CoreLib.NOT);    
    bind(new Symbol("gensym"), CoreLib.GENSYM);
    bind(new Symbol("identity-hash"), CoreLib.IDENTITY_HASH);
    bind(new Symbol("error"), CoreLib.ERROR);    
    bind(new Symbol("format-decimal"), CoreLib.FORMAT_DECIMAL);
    bind(new Symbol("max"), CoreLib.MAX);
    bind(new Symbol("min"), CoreLib.MIN);
    
    /* Type predicates */
    
    bind(new Symbol("nil?"), CoreLib.IS_NIL);
    bind(new Symbol("empty?"), CoreLib.IS_EMPTY_LIST);
    bind(new Symbol("boolean?"), CoreLib.IS_BOOL);
    bind(new Symbol("integer?"), CoreLib.IS_INT);
    bind(new Symbol("real?"), CoreLib.IS_REAL);
    bind(new Symbol("complex?"), CoreLib.IS_COMPLEX);
    bind(new Symbol("number?"), CoreLib.IS_NUMBER);
    bind(new Symbol("symbol?"), CoreLib.IS_SYMBOL);
    bind(new Symbol("text?"), CoreLib.IS_TEXT);
    //bind(new Symbol("bytes?"), CoreLib.IsBytes);
    //bind(new Symbol("list?"), CoreLib.IsList);
    //bind(new Symbol("vector?"), CoreLib.IsVector);
    bind(new Symbol("callable?"), CoreLib.IS_CALLABLE);
    
    /* Math constants & procedures */
    
    bind(new Symbol("E"), Real.E);
    bind(new Symbol("PI"), Real.PI);
    bind(new Symbol("+inf"), Real.POS_INF);
    bind(new Symbol("-inf"), Real.NEG_INF);
    bind(new Symbol("NaN"), Real.NAN);
    bind(new Symbol("I"), Complex.I);    
    bind(new Symbol("+"), MathLib.ADD);
    bind(new Symbol("-"), MathLib.SUB);
    bind(new Symbol("*"), MathLib.MUL);
    bind(new Symbol("/"), MathLib.DIV);
    bind(new Symbol("~"), MathLib.NEG);
    bind(new Symbol("abs"), MathLib.ABS);
    bind(new Symbol("floor"), MathLib.FLOOR);
    bind(new Symbol("ceiling"), MathLib.CEILING);
    bind(new Symbol("round"), MathLib.ROUND);
    bind(new Symbol("truncate"), MathLib.TRUNCATE);
    bind(new Symbol("quotient"), MathLib.QUOTIENT);
    bind(new Symbol("remainder"), MathLib.REMAINDER);
    bind(new Symbol("exp"), MathLib.EXP);
    bind(new Symbol("log"), MathLib.LOG);
    bind(new Symbol("sin"), MathLib.SIN);
    bind(new Symbol("cos"), MathLib.COS);
    bind(new Symbol("tan"), MathLib.TAN);
    bind(new Symbol("asin"), MathLib.ASIN);
    bind(new Symbol("acos"), MathLib.ACOS);
    bind(new Symbol("atan"), MathLib.ATAN);    
    bind(new Symbol("rand"), MathLib.RAND);
    bind(new Symbol("complex"), MathLib.MAKE_COMPLEX);
    bind(new Symbol("real-part"), MathLib.REAL_PART);
    bind(new Symbol("imag-part"), MathLib.IMAG_PART);
    bind(new Symbol("rect->polar"), MathLib.RECT_TO_POLAR);
    bind(new Symbol("polar->rect"), MathLib.POLAR_TO_RECT);
    bind(new Symbol("ratio"), MathLib.MAKE_RATIO);
    bind(new Symbol("numerator"), MathLib.NUMERATOR);
    bind(new Symbol("denominator"), MathLib.DENOMINATOR);
    
    /* Conversion procedures */
    
    bind(new Symbol("text->symbol"), CoreLib.TEXT_TO_SYMBOL);
    bind(new Symbol("symbol->text"), CoreLib.SYMBOL_TO_TEXT); 
    bind(new Symbol("text->bytes"), CoreLib.TEXT_TO_BYTES); // encode
    bind(new Symbol("bytes->text"), CoreLib.BYTES_TO_TEXT); // decode
    
    /* List procedures */

    bind(new Symbol("list"), ListLib.MAKE_LIST);
    bind(new Symbol("cons"), ListLib.CONS);
    bind(new Symbol("car"), ListLib.CAR);
    bind(new Symbol("caar"), ListLib.CAAR);
    bind(new Symbol("cadr"), ListLib.CADR);
    bind(new Symbol("caddr"), ListLib.CADDR);
    bind(new Symbol("cdr"), ListLib.CDR);
    bind(new Symbol("cddr"), ListLib.CDDR);
    bind(new Symbol("cdddr"), ListLib.CDDDR);
    bind(new Symbol("concat"), ListLib.CONCAT);
    bind(new Symbol("append"), ListLib.APPEND);
    bind(new Symbol("reverse"), ListLib.REVERSE);
    bind(new Symbol("set-car!"), ListLib.SET_CAR);
    bind(new Symbol("set-cdr!"), ListLib.SET_CDR);
    
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
    
    bind(new Symbol("port/open"), PortLib.OPEN);
    bind(new Symbol("port/close"), PortLib.CLOSE);
    bind(new Symbol("port/read"), PortLib.READ);
    bind(new Symbol("port/write"), PortLib.WRITE);
    bind(new Symbol("port/stdin"), InputPort.STDIN);
    bind(new Symbol("port/stdout"), OutputPort.STDOUT);
    
    /* Bytes procedures */
    bind(new Symbol("bytes/new"), CoreLib.MAKE_BYTES);
  }
}
