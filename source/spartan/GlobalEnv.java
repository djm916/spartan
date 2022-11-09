package spartan;

import spartan.data.*;
import spartan.builtins.CoreLib;
import spartan.builtins.MathLib;
import spartan.builtins.ListLib;
import spartan.builtins.VectorLib;
import spartan.builtins.MappingLib;
import spartan.builtins.StringLib;
import spartan.builtins.PortLib;
import spartan.builtins.BytesLib;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import spartan.errors.UnboundVariable;

public final class GlobalEnv
{
  public static GlobalEnv createBasis()
  {
    return new GlobalEnv();
  }
  
  /** Bind a variable to a given value.
      If the variable is already bound, its value is replaced.
      @param name The variable to bind
  */
  public void bind(Symbol name, Datum val)
  {
    globals.put(name, val);
  }
  
  /** Lookup a variable by name, optionally returning its bound value.
      @param name The variable to look up
      @return The (optional) value of the variable
  */
  public Optional<Datum> lookup(Symbol name)
  {
    return Optional.ofNullable(globals.get(name));
  }
  
  /** Lookup a variable by name, throwing an exception if it is not bound.
      @param name The variable to look up
      @return The value of the variable
  */
  public Datum lookupOrElse(Symbol name, Datum defaultValue)
  {
    return globals.getOrDefault(name, defaultValue);
  }
  
  public Datum lookupOrThrow(Symbol name)
  {
    var value = globals.get(name);
    if (value == null)
      throw new UnboundVariable(name);
    return value;
  }
  
  private GlobalEnv() {}
  
  private final Map<Symbol, Datum> globals = new IdentityHashMap<>();
  
  {
    /* General values & procedures */
    
    bind(Symbol.of("nil"), Nil.VALUE);
    bind(Symbol.of("true"), Bool.TRUE);
    bind(Symbol.of("false"), Bool.FALSE);    
    bind(Symbol.of("apply"), CoreLib.APPLY);
    bind(Symbol.of("print"), CoreLib.PRINT);
    bind(Symbol.of("print-line"), CoreLib.PRINT_LINE);
    bind(Symbol.of("type"), CoreLib.TYPE);
    bind(Symbol.of("length"), CoreLib.LENGTH);
    bind(Symbol.of("load"), CoreLib.LOAD);
    bind(Symbol.of("="), CoreLib.EQ);
    bind(Symbol.of("/="), CoreLib.NE);
    bind(Symbol.of("<"), CoreLib.LT);
    bind(Symbol.of(">"), CoreLib.GT);
    bind(Symbol.of("<="), CoreLib.LE);
    bind(Symbol.of(">="), CoreLib.GE);
    bind(Symbol.of("not"), CoreLib.NOT);    
    bind(Symbol.of("gensym"), CoreLib.GENSYM);
    bind(Symbol.of("identity-hash"), CoreLib.IDENTITY_HASH);
    bind(Symbol.of("error"), CoreLib.ERROR);    
    bind(Symbol.of("format-decimal"), CoreLib.FORMAT_DECIMAL);
    bind(Symbol.of("max"), CoreLib.MAX);
    bind(Symbol.of("min"), CoreLib.MIN);
    
    /* Type predicates */
    
    bind(Symbol.of("nil?"), CoreLib.IS_NIL);
    bind(Symbol.of("boolean?"), CoreLib.IS_BOOL);
    bind(Symbol.of("integer?"), CoreLib.IS_INT);
    bind(Symbol.of("real?"), CoreLib.IS_REAL);
    bind(Symbol.of("complex?"), CoreLib.IS_COMPLEX);
    bind(Symbol.of("number?"), CoreLib.IS_NUMBER);
    bind(Symbol.of("symbol?"), CoreLib.IS_SYMBOL);
    bind(Symbol.of("text?"), CoreLib.IS_TEXT);
    //bind(Symbol.of("bytes?"), CoreLib.IsBytes);
    bind(Symbol.of("empty?"), CoreLib.IS_EMPTY_LIST);
    bind(Symbol.of("list?"), CoreLib.IS_LIST);
    bind(Symbol.of("vector?"), CoreLib.IS_VECTOR);
    bind(Symbol.of("callable?"), CoreLib.IS_CALLABLE);
    
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
    bind(Symbol.of("real-part"), MathLib.REAL_PART);
    bind(Symbol.of("imag-part"), MathLib.IMAG_PART);
    bind(Symbol.of("rect->polar"), MathLib.RECT_TO_POLAR);
    bind(Symbol.of("polar->rect"), MathLib.POLAR_TO_RECT);
    bind(Symbol.of("ratio"), MathLib.MAKE_RATIO);
    bind(Symbol.of("numerator"), MathLib.NUMERATOR);
    bind(Symbol.of("denominator"), MathLib.DENOMINATOR);
        
    /* Conversion procedures */
    
    bind(Symbol.of("string->symbol"), CoreLib.TEXT_TO_SYMBOL);
    bind(Symbol.of("symbol->string"), CoreLib.SYMBOL_TO_TEXT); 
    bind(Symbol.of("string->bytes"), CoreLib.TEXT_TO_BYTES); // encode
    bind(Symbol.of("bytes->string"), CoreLib.BYTES_TO_TEXT); // decode
    //bind(Symbol.of("string->number"), CoreLib.TEXT_TO_NUMBER);
    
    /* List procedures */

    bind(Symbol.of("list"), ListLib.MAKE_LIST);
    bind(Symbol.of("cons"), ListLib.CONS);
    bind(Symbol.of("car"), ListLib.CAR);
    bind(Symbol.of("caar"), ListLib.CAAR);
    bind(Symbol.of("cadr"), ListLib.CADR);
    bind(Symbol.of("caddr"), ListLib.CADDR);
    bind(Symbol.of("cdr"), ListLib.CDR);
    bind(Symbol.of("cddr"), ListLib.CDDR);
    bind(Symbol.of("cdddr"), ListLib.CDDDR);
    bind(Symbol.of("concat"), ListLib.CONCAT);
    bind(Symbol.of("append"), ListLib.APPEND);
    bind(Symbol.of("reverse"), ListLib.REVERSE);
    //bind(Symbol.of("remove!"), ListLib.REMOVE);
    //bind(Symbol.of("remove"), ListLib.REMOVED);
    bind(Symbol.of("set-car!"), ListLib.SET_CAR);
    bind(Symbol.of("set-cdr!"), ListLib.SET_CDR);
    
    /* Vector procedures */
    
    bind(Symbol.of("vector"), VectorLib.FROM_LIST);
    bind(Symbol.of("vector/fill"), VectorLib.FILL);
    bind(Symbol.of("vector/length"), VectorLib.LENGTH);
    bind(Symbol.of("vector/copy"), VectorLib.COPY);
    bind(Symbol.of("vector/get"), VectorLib.GET);
    bind(Symbol.of("vector/set!"), VectorLib.SET);
    bind(Symbol.of("vector/append!"), VectorLib.APPEND);
    
    /* Mapping procedures */
    
    bind(Symbol.of("mapping"), MappingLib.FROM_LIST);
    bind(Symbol.of("mapping/get"), MappingLib.GET);
    bind(Symbol.of("mapping/set!"), MappingLib.SET);
    bind(Symbol.of("mapping/length"), MappingLib.LENGTH);
    bind(Symbol.of("mapping/keys"), MappingLib.KEYS);
    bind(Symbol.of("mapping/values"), MappingLib.VALUES);
    bind(Symbol.of("mapping/entries"), MappingLib.ENTRIES);
    
    
    /* String procedures */
    
    bind(Symbol.of("string/concat"), StringLib.CONCAT);
    bind(Symbol.of("string/join"), StringLib.JOIN);
    bind(Symbol.of("string/substr"), StringLib.SUBSTR);
    bind(Symbol.of("string/reverse"), StringLib.REVERSE);
    bind(Symbol.of("string/hash"), StringLib.HASH);
    
    /* Port procedures */
    
    bind(Symbol.of("port/open"), PortLib.OPEN);
    bind(Symbol.of("port/close"), PortLib.CLOSE);
    bind(Symbol.of("port/read"), PortLib.READ);
    bind(Symbol.of("port/write"), PortLib.WRITE);
    bind(Symbol.of("port/stdin"), InputPort.STDIN);
    bind(Symbol.of("port/stdout"), OutputPort.STDOUT);
    
    /* Bytes procedures */
    
    bind(Symbol.of("bytes"), BytesLib.FROM_LIST);
    bind(Symbol.of("bytes/new"), BytesLib.MAKE_BYTES);
    bind(Symbol.of("bytes/ref"), BytesLib.REF);
    bind(Symbol.of("bytes/set!"), BytesLib.SET);
    bind(Symbol.of("bytes/push!"), BytesLib.PUSH);
    bind(Symbol.of("bytes/pop!"), BytesLib.POP);
    bind(Symbol.of("bytes/flip!"), BytesLib.FLIP);
    bind(Symbol.of("bytes/clear!"), BytesLib.FLIP);
    bind(Symbol.of("bytes/remaining"), BytesLib.REMAINING);
    bind(Symbol.of("bytes/empty?"), BytesLib.IS_EMPTY);
  }
}
