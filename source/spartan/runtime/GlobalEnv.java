package spartan.runtime;

import spartan.data.*;
import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.TableLib;
import spartan.builtins.StringLib;
import spartan.builtins.PortLib;
import spartan.builtins.BytesLib;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import spartan.errors.UnboundVariable;

public final class GlobalEnv
{
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public static void bind(Symbol name, Datum val)
  {
    bindings.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public static Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(bindings.get(name));
  }
  
  /** Lookup a variable by name, throwing an exception if it is not bound.
      @param name The variable to look up
      @return The value of the variable
  */
  public static Datum lookupOrElse(Symbol name, Datum defaultValue)
  {
    return bindings.getOrDefault(name, defaultValue);
  }
  
  public static Datum lookupOrThrow(Symbol name)
  {
    var value = bindings.get(name);
    if (value == null)
      throw new UnboundVariable(name);
    return value;
  }
  
  private GlobalEnv() { }
  
  private static final Map<Symbol, Datum> bindings = new IdentityHashMap<>();
  
  static
  {
    /* General values & procedures */
    
    bind(Symbol.of("="), CoreLib.EQ);
    bind(Symbol.of("/="), CoreLib.NE);
    bind(Symbol.of("<"), CoreLib.LT);
    bind(Symbol.of(">"), CoreLib.GT);
    bind(Symbol.of("<="), CoreLib.LE);
    bind(Symbol.of(">="), CoreLib.GE);
    bind(Symbol.of("nil"), Nil.VALUE);
    bind(Symbol.of("true"), Bool.TRUE);
    bind(Symbol.of("false"), Bool.FALSE);    
    bind(Symbol.of("apply"), CoreLib.APPLY);
    bind(Symbol.of("print"), CoreLib.PRINT);
    bind(Symbol.of("print-line"), CoreLib.PRINT_LINE);
    bind(Symbol.of("type"), CoreLib.TYPE);    
    bind(Symbol.of("load"), CoreLib.LOAD);
    bind(Symbol.of("not"), CoreLib.NOT);    
    bind(Symbol.of("gensym"), CoreLib.GENSYM);
    bind(Symbol.of("identity-hash"), CoreLib.IDENTITY_HASH);
    bind(Symbol.of("error"), CoreLib.ERROR);
    bind(Symbol.of("at"), CoreLib.AT);
    bind(Symbol.of("set-at!"), CoreLib.SET_AT);
    bind(Symbol.of("length"), CoreLib.LENGTH);
    bind(Symbol.of("empty?"), CoreLib.IS_EMPTY);
    
    /* Type predicates */
    
    bind(Symbol.of("nil?"), CoreLib.IS_NIL);
    bind(Symbol.of("boolean?"), CoreLib.IS_BOOL);
    bind(Symbol.of("integer?"), CoreLib.IS_INT);
    bind(Symbol.of("real?"), CoreLib.IS_REAL);
    bind(Symbol.of("complex?"), CoreLib.IS_COMPLEX);
    bind(Symbol.of("number?"), CoreLib.IS_NUMBER);
    bind(Symbol.of("symbol?"), CoreLib.IS_SYMBOL);
    bind(Symbol.of("string?"), CoreLib.IS_TEXT);
    bind(Symbol.of("list?"), CoreLib.IS_LIST);
    bind(Symbol.of("vector?"), CoreLib.IS_VECTOR);
    bind(Symbol.of("callable?"), CoreLib.IS_CALLABLE);
    bind(Symbol.of("port?"), CoreLib.IS_PORT);
    bind(Symbol.of("bytes?"), CoreLib.IS_BYTES);
    
    /* Math constants & procedures */

    bind(Symbol.of("E"), Real.E);
    bind(Symbol.of("PI"), Real.PI);
    bind(Symbol.of("+inf"), Real.POS_INF);
    bind(Symbol.of("-inf"), Real.NEG_INF);
    bind(Symbol.of("NaN"), Real.NAN);
    bind(Symbol.of("I"), Complex.I);    
    bind(Symbol.of("+"), MathLib.ADD);
    bind(Symbol.of("-"), MathLib.SUB);
    bind(Symbol.of("*"), MathLib.MUL);
    bind(Symbol.of("/"), MathLib.DIV);
    bind(Symbol.of("~"), MathLib.NEG);
    bind(Symbol.of("abs"), MathLib.ABS);
    bind(Symbol.of("floor"), MathLib.FLOOR);
    bind(Symbol.of("ceiling"), MathLib.CEILING);
    bind(Symbol.of("round"), MathLib.ROUND);
    //bind(Symbol.of("truncate"), MathLib.TRUNC);
    bind(Symbol.of("quotient"), MathLib.QUOTIENT);
    bind(Symbol.of("remainder"), MathLib.REMAINDER);
    bind(Symbol.of("exp"), MathLib.EXP);
    bind(Symbol.of("log"), MathLib.LOG);
    bind(Symbol.of("sin"), MathLib.SIN);
    bind(Symbol.of("cos"), MathLib.COS);
    bind(Symbol.of("tan"), MathLib.TAN);
    bind(Symbol.of("asin"), MathLib.ASIN);
    bind(Symbol.of("acos"), MathLib.ACOS);
    bind(Symbol.of("atan"), MathLib.ATAN);    
    bind(Symbol.of("rand"), MathLib.RAND);
    bind(Symbol.of("complex"), MathLib.MAKE_COMPLEX);
    bind(Symbol.of("real"), MathLib.REAL);
    bind(Symbol.of("imag"), MathLib.IMAG);
    bind(Symbol.of("angle"), MathLib.ANGLE);
    bind(Symbol.of("magnitude"), MathLib.MAGNITUDE);
    bind(Symbol.of("ratio"), MathLib.MAKE_RATIO);
    bind(Symbol.of("numer"), MathLib.NUMERATOR);
    bind(Symbol.of("denom"), MathLib.DENOMINATOR);
        
    /* Conversion procedures */
    
    bind(Symbol.of("string->symbol"), CoreLib.TEXT_TO_SYMBOL);
    bind(Symbol.of("symbol->string"), CoreLib.SYMBOL_TO_TEXT); 
    bind(Symbol.of("string->bytes"), CoreLib.TEXT_TO_BYTES); // encode
    bind(Symbol.of("bytes->string"), CoreLib.BYTES_TO_TEXT); // decode
    //bind(Symbol.of("string->number"), CoreLib.TEXT_TO_NUMBER);
    bind(Symbol.of("string->int"), CoreLib.TEXT_TO_INT);
    bind(Symbol.of("format-int"), CoreLib.FORMAT_INT);
    bind(Symbol.of("format-decimal"), CoreLib.FORMAT_DECIMAL);
    
    /* List procedures */

    bind(Symbol.of("list"), ListLib.MAKE_LIST);
    bind(Symbol.of("cons"), ListLib.CONS);
    bind(Symbol.of("car"), ListLib.CAR);
    bind(Symbol.of("cdr"), ListLib.CDR);
    bind(Symbol.of("cddr"), ListLib.CDDR);
    bind(Symbol.of("cdddr"), ListLib.CDDDR);
    bind(Symbol.of("cadr"), ListLib.CADR);
    bind(Symbol.of("caddr"), ListLib.CADDR);
    bind(Symbol.of("concat"), ListLib.CONCAT);
    bind(Symbol.of("append"), ListLib.APPEND);
    bind(Symbol.of("reverse"), ListLib.REVERSE);
    //bind(Symbol.of("remove!"), ListLib.REMOVE);
    //bind(Symbol.of("remove"), ListLib.REMOVED);
    bind(Symbol.of("set-car!"), ListLib.SET_CAR);
    bind(Symbol.of("set-cdr!"), ListLib.SET_CDR);
    //bind(Symbol.of("length"), ListLib.LENGTH);
    //bind(Symbol.of("empty?"), ListLib.IS_EMPTY);
    //bind(Symbol.of("nth"), ListLib.NTH);
    //bind(Symbol.of("set-nth!"), ListLib.SET_NTH);
    bind(Symbol.of("nth-tail"), ListLib.NTH_TAIL);
    bind(Symbol.of("set-nth-tail!"), ListLib.SET_NTH_TAIL);
    
    /* Vector procedures */
    
    bind(Symbol.of("vector"), VectorLib.FROM_LIST);
    bind(Symbol.of("vector:new"), VectorLib.NEW);
    //bind(Symbol.of("vector:ref"), VectorLib.REF);    
    //bind(Symbol.of("vector:length"), VectorLib.LENGTH);
    bind(Symbol.of("vector:copy"), VectorLib.COPY);
    //bind(Symbol.of("vector:set!"), VectorLib.SET);
    bind(Symbol.of("vector:append!"), VectorLib.APPEND);
    bind(Symbol.of("vector:insert!"), VectorLib.INSERT);
    bind(Symbol.of("vector:remove!"), VectorLib.REMOVE);
    
    /* Table procedures */
    
    bind(Symbol.of("table"), TableLib.FROM_LIST);
    //bind(Symbol.of("table:find"), TableLib.FIND);
    //bind(Symbol.of("table:assoc!"), TableLib.ASSOC);
    //bind(Symbol.of("table:length"), TableLib.LENGTH);
    bind(Symbol.of("table:keys"), TableLib.KEYS);
    bind(Symbol.of("table:values"), TableLib.VALUES);
    bind(Symbol.of("table:entries"), TableLib.ENTRIES);
    
    /* String procedures */
    
    //bind(Symbol.of("string:length"), StringLib.LENGTH);
    bind(Symbol.of("string:concat"), StringLib.CONCAT);
    bind(Symbol.of("string:join"), StringLib.JOIN);
    bind(Symbol.of("string:substr"), StringLib.SUBSTR);
    bind(Symbol.of("string:reverse"), StringLib.REVERSE);
    bind(Symbol.of("string:hash"), StringLib.HASH);
    //bind(Symbol.of("string/compare"), StringLib.COMPARE);
    //bind(Symbol.of("string/compare-no-case"), StringLib.COMPARE_NO_CASE);
    
    /* Port procedures */
    
    bind(Symbol.of("port:open"), PortLib.OPEN);
    bind(Symbol.of("port:close"), PortLib.CLOSE);
    bind(Symbol.of("port:read"), PortLib.READ);
    bind(Symbol.of("port:write"), PortLib.WRITE);
    bind(Symbol.of("port:stdin"), InputPort.STDIN);
    bind(Symbol.of("port:stdout"), OutputPort.STDOUT);
    bind(Symbol.of("port:open?"), PortLib.IS_OPEN);
    bind(Symbol.of("port:position"), PortLib.POSITION);
    bind(Symbol.of("port:seek"), PortLib.SEEK);
    bind(Symbol.of("port:length"), PortLib.LENGTH);
    
    /* Bytes procedures */
    
    bind(Symbol.of("bytes"), BytesLib.FROM_LIST);
    bind(Symbol.of("bytes:new"), BytesLib.NEW);
    //bind(Symbol.of("bytes:ref"), BytesLib.REF);
    //bind(Symbol.of("bytes:set!"), BytesLib.SET);
    //bind(Symbol.of("bytes:length"), BytesLib.LENGTH);
  }
}
