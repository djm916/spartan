package spartan.builtins;

import spartan.data.*;
import spartan.data.Module;
import java.util.Optional;
import java.util.logging.Logger;

/**
 * The "spartan.base" module
 */
public final class BaseModule extends Module
{
  private static final Logger log = Logger.getLogger(BaseModule.class.getName());
  
  public static final String NAME = "spartan.base";
  
  public static final BaseModule INSTANCE = new BaseModule();
  
  private BaseModule()
  {
    super(Symbol.of(NAME), null);
  }
  
  {
    // populate this module
    
    if (spartan.Config.LOG_DEBUG)
      log.info(() -> String.format("initializing module %s", NAME));
    
    bindPublic(Symbol.of("="), BaseLib.EQ);
    bindPublic(Symbol.of("/="), BaseLib.NE);
    bindPublic(Symbol.of("<"), BaseLib.LT);
    bindPublic(Symbol.of(">"), BaseLib.GT);
    bindPublic(Symbol.of("<="), BaseLib.LE);
    bindPublic(Symbol.of(">="), BaseLib.GE);
    bindPublic(Symbol.of("not"), BaseLib.NOT);    
    bindPublic(Symbol.of("apply"), BaseLib.APPLY);
    bindPublic(Symbol.of("call/cc"), BaseLib.CALL_CC);
    bindPublic(Symbol.of("print"), BaseLib.PRINT);
    bindPublic(Symbol.of("print-line"), BaseLib.PRINT_LINE);
    bindPublic(Symbol.of("type"), BaseLib.TYPE);    
    bindPublic(Symbol.of("load"), BaseLib.LOAD);    
    bindPublic(Symbol.of("gensym"), BaseLib.GENSYM);    
    bindPublic(Symbol.of("error"), BaseLib.ERROR);
    bindPublic(Symbol.of("macroexpand-1"), BaseLib.MACROEXPAND1);
    bindPublic(Symbol.of("identity-hash"), BaseLib.IDENTITY_HASH);
    bindPublic(Symbol.of("identical?"), BaseLib.IS_IDENTICAL);
    bindPublic(Symbol.of("current-time-ms"), BaseLib.CURRENT_TIME_MS);
    
    /* Type predicates */
    
    bindPublic(Symbol.of("nil?"), BaseLib.IS_NIL);
    bindPublic(Symbol.of("boolean?"), BaseLib.IS_BOOL);
    bindPublic(Symbol.of("integer?"), BaseLib.IS_INT);
    bindPublic(Symbol.of("real?"), BaseLib.IS_REAL);
    bindPublic(Symbol.of("complex?"), BaseLib.IS_COMPLEX);
    bindPublic(Symbol.of("number?"), BaseLib.IS_NUMBER);
    bindPublic(Symbol.of("symbol?"), BaseLib.IS_SYMBOL);
    bindPublic(Symbol.of("string?"), BaseLib.IS_TEXT);
    bindPublic(Symbol.of("list?"), BaseLib.IS_LIST);
    bindPublic(Symbol.of("vector?"), BaseLib.IS_VECTOR);
    bindPublic(Symbol.of("callable?"), BaseLib.IS_CALLABLE);
    bindPublic(Symbol.of("port?"), BaseLib.IS_PORT);
    bindPublic(Symbol.of("bytes?"), BaseLib.IS_BYTES);
    bindPublic(Symbol.of("record?"), BaseLib.IS_RECORD);
    
    /* Math constants & procedures */

    bindPublic(Symbol.of("E"), Real.E);
    bindPublic(Symbol.of("PI"), Real.PI);
    bindPublic(Symbol.of("+inf"), Real.POS_INF);
    bindPublic(Symbol.of("-inf"), Real.NEG_INF);
    bindPublic(Symbol.of("NaN"), Real.NAN);
    bindPublic(Symbol.of("I"), Complex.I);    
    bindPublic(Symbol.of("+"), MathLib.ADD);
    bindPublic(Symbol.of("-"), MathLib.SUB);
    bindPublic(Symbol.of("*"), MathLib.MUL);
    bindPublic(Symbol.of("/"), MathLib.DIV);
    bindPublic(Symbol.of("~"), MathLib.NEG);
    bindPublic(Symbol.of("abs"), MathLib.ABS);
    bindPublic(Symbol.of("floor"), MathLib.FLOOR);
    bindPublic(Symbol.of("ceiling"), MathLib.CEILING);
    bindPublic(Symbol.of("round"), MathLib.ROUND);
    //bindPublic(Symbol.of("truncate"), MathLib.TRUNC);
    bindPublic(Symbol.of("quotient"), MathLib.QUOTIENT);
    bindPublic(Symbol.of("remainder"), MathLib.REMAINDER);
    bindPublic(Symbol.of("exp"), MathLib.EXP);
    bindPublic(Symbol.of("log"), MathLib.LOG);
    bindPublic(Symbol.of("sin"), MathLib.SIN);
    bindPublic(Symbol.of("cos"), MathLib.COS);
    bindPublic(Symbol.of("tan"), MathLib.TAN);
    bindPublic(Symbol.of("asin"), MathLib.ASIN);
    bindPublic(Symbol.of("acos"), MathLib.ACOS);
    bindPublic(Symbol.of("atan"), MathLib.ATAN);    
    bindPublic(Symbol.of("rand"), MathLib.RAND);
    bindPublic(Symbol.of("complex"), MathLib.MAKE_COMPLEX);
    bindPublic(Symbol.of("real"), MathLib.REAL);
    bindPublic(Symbol.of("imag"), MathLib.IMAG);
    bindPublic(Symbol.of("angle"), MathLib.ANGLE);
    bindPublic(Symbol.of("magnitude"), MathLib.MAGNITUDE);
    bindPublic(Symbol.of("ratio"), MathLib.MAKE_RATIO);
    bindPublic(Symbol.of("numer"), MathLib.NUMERATOR);
    bindPublic(Symbol.of("denom"), MathLib.DENOMINATOR);
    
    /* Conversion procedures */
    
    bindPublic(Symbol.of("string->symbol"), BaseLib.TEXT_TO_SYMBOL);
    bindPublic(Symbol.of("symbol->string"), BaseLib.SYMBOL_TO_TEXT);
    bindPublic(Symbol.of("string->bytes"), BaseLib.STRING_TO_BYTES); // encode
    bindPublic(Symbol.of("bytes->string"), BaseLib.BYTES_TO_STRING); // decode
    //bindPublic(Symbol.of("string->number"), BaseLib.TEXT_TO_NUMBER);
    bindPublic(Symbol.of("string->int"), BaseLib.TEXT_TO_INT);
    bindPublic(Symbol.of("format-int"), BaseLib.FORMAT_INT);
    bindPublic(Symbol.of("format-decimal"), BaseLib.FORMAT_DECIMAL);
    
    /* List procedures */
        
    bindPublic(Symbol.of("adjoin"), ListLib.ADJOIN);
    bindPublic(Symbol.of("first"), ListLib.FIRST);
    bindPublic(Symbol.of("second"), ListLib.SECOND);
    bindPublic(Symbol.of("third"), ListLib.THIRD);
    //bindPublic(Symbol.of("fourth"), ListLib.FOURTH);
    bindPublic(Symbol.of("rest"), ListLib.REST);
    bindPublic(Symbol.of("empty?"), ListLib.IS_EMPTY);
    bindPublic(Symbol.of("set-first!"), ListLib.SET_FIRST);
    bindPublic(Symbol.of("set-rest!"), ListLib.SET_REST);
    bindPublic(Symbol.of("list"), ListLib.MAKE_LIST);
    bindPublic(Symbol.of("length"), ListLib.LENGTH);
    bindPublic(Symbol.of("reverse"), ListLib.REVERSE);
    bindPublic(Symbol.of("concat"), ListLib.CONCAT);
    bindPublic(Symbol.of("append"), ListLib.APPEND);
    //bindPublic(Symbol.of("take"), ListLib.TAKE);
    //bindPublic(Symbol.of("drop"), ListLib.DROP);
    bindPublic(Symbol.of("nth"), ListLib.NTH);
    bindPublic(Symbol.of("nth-rest"), ListLib.DROP);
    
    /* Vector procedures */
    
    bindPublic(Symbol.of("vector"), VectorLib.FROM_LIST);
    bindPublic(Symbol.of("make-vector"), VectorLib.MAKE);
    bindPublic(Symbol.of("vector-ref"), VectorLib.REF);
    bindPublic(Symbol.of("vector-set!"), VectorLib.SET);
    bindPublic(Symbol.of("vector-length"), VectorLib.LENGTH);
    bindPublic(Symbol.of("vector-copy"), VectorLib.COPY);
    bindPublic(Symbol.of("vector-append!"), VectorLib.APPEND);
    bindPublic(Symbol.of("vector-insert!"), VectorLib.INSERT);
    bindPublic(Symbol.of("vector-remove!"), VectorLib.REMOVE);
    
    /* String & String Cursor procedures */
    
    bindPublic(Symbol.of("string"), StringLib.FROM_LIST);
    bindPublic(Symbol.of("string-ref"), StringLib.REF);
    bindPublic(Symbol.of("string-length"), StringLib.LENGTH);
    bindPublic(Symbol.of("string-empty?"), StringLib.IS_EMPTY);
    bindPublic(Symbol.of("string-substring"), StringLib.SUBSTR);
    bindPublic(Symbol.of("string-concat"), StringLib.CONCAT);
    bindPublic(Symbol.of("string-join"), StringLib.JOIN);
    bindPublic(Symbol.of("string-split"), StringLib.SPLIT);    
    bindPublic(Symbol.of("string-reverse"), StringLib.REVERSE);
    bindPublic(Symbol.of("string-find"), StringLib.FIND);
    bindPublic(Symbol.of("string-replace"), StringLib.REPLACE);
    bindPublic(Symbol.of("string-insert"), StringLib.INSERT);
    bindPublic(Symbol.of("string-delete"), StringLib.DELETE);
    bindPublic(Symbol.of("string-hash"), StringLib.HASH);
    bindPublic(Symbol.of("string-cursor-begin"), StringLib.CURSOR_BEGIN);
    bindPublic(Symbol.of("string-cursor-end"), StringLib.CURSOR_END);
    bindPublic(Symbol.of("string-cursor-next"), StringLib.CURSOR_NEXT);
    bindPublic(Symbol.of("string-cursor-prev"), StringLib.CURSOR_PREV);
        
    /* I/O and Port procedures */
    
    bindPublic(Symbol.of("*standard-input-port*"), InputPort.STDIN);
    bindPublic(Symbol.of("*standard-output-port*"), OutputPort.STDOUT);
    bindPublic(Symbol.of("*standard-error-port*"), OutputPort.STDERR);
    bindPublic(Symbol.of("port-open-file"), PortLib.OPEN);
    bindPublic(Symbol.of("port-close"), PortLib.CLOSE);
    bindPublic(Symbol.of("port-read"), PortLib.READ);
    bindPublic(Symbol.of("port-write"), PortLib.WRITE);    
    bindPublic(Symbol.of("port-open?"), PortLib.IS_OPEN);
    bindPublic(Symbol.of("port-position"), PortLib.POSITION);
    bindPublic(Symbol.of("port-seek"), PortLib.SEEK);
    bindPublic(Symbol.of("port-size"), PortLib.SIZE);
    
    /* Bytevector related procedures */
    
    bindPublic(Symbol.of("make-bytes"), BytesLib.MAKE);
    bindPublic(Symbol.of("bytes-ref"), BytesLib.REF);
    bindPublic(Symbol.of("bytes-set!"), BytesLib.SET);
    bindPublic(Symbol.of("bytes-length"), BytesLib.LENGTH);
    //bindPublic(Symbol.of("bytes-write-int32"), BytesLib.WRITE_INT32);
    
    /* Record related procedures */
    
    bindPublic(Symbol.of("make-record-type"), BaseLib.MAKE_RECORD_TYPE);
    bindPublic(Symbol.of("record-constructor"), BaseLib.RECORD_CONSTRUCTOR);
    bindPublic(Symbol.of("record-predicate"), BaseLib.RECORD_PREDICATE);
    bindPublic(Symbol.of("record-accessor"), BaseLib.RECORD_ACCESSOR);
    bindPublic(Symbol.of("record-mutator"), BaseLib.RECORD_MUTATOR);    
    bindPublic(Symbol.of("record-descriptor"), BaseLib.GET_DESCRIPTOR);
    
    /* Symbol related procedures */
    
    bindPublic(Symbol.of("symbol-intern"), BaseLib.SYMBOL_INTERN);
    bindPublic(Symbol.of("make-symbol"), BaseLib.MAKE_SYMBOL);
    bindPublic(Symbol.of("symbol-modulename"), BaseLib.SYMBOL_MODULENAME);
    bindPublic(Symbol.of("symbol-basename"), BaseLib.SYMBOL_BASENAME);
    bindPublic(Symbol.of("symbol-qualified?"), BaseLib.SYMBOL_IS_QUALIFIED);
    
    /* Module related procedures */
    
    bindPublic(Symbol.of("current-module"), BaseLib.CURRENT_MODULE);
    bindPublic(Symbol.of("set-current-module!"), BaseLib.SET_CURRENT_MODULE);
    bindPublic(Symbol.of("make-module"), BaseLib.MAKE_MODULE);
    bindPublic(Symbol.of("find-module"), BaseLib.FIND_MODULE);
    bindPublic(Symbol.of("the-module"), BaseLib.THE_MODULE);
    bindPublic(Symbol.of("module-symbols"), BaseLib.MODULE_SYMBOLS);
    bindPublic(Symbol.of("module-alias"), BaseLib.MODULE_ALIAS);
    //bindPublic(Symbol.of("module-bind"), BaseLib.MODULE_BIND);
    //bindPublic(Symbol.of("module-resolve"), BaseLib.MODULE_RESOLVE);
    bindPublic(Symbol.of("module-import"), BaseLib.MODULE_IMPORT);
    bindPublic(Symbol.of("module-export"), BaseLib.MODULE_EXPORT);
    bindPublic(Symbol.of("module-name->path"), BaseLib.MODULENAME_TO_PATH);
  }
}
