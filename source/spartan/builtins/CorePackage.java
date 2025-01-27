package spartan.builtins;

import spartan.data.*;
import java.util.Optional;
import java.util.logging.Logger;

/**
 * The spartan.core package.
 */
public final class CorePackage extends spartan.data.Package
{
  private static final Logger log = Logger.getLogger(CorePackage.class.getName());
  
  public static final String NAME = "spartan.core";
  
  public static final CorePackage INSTANCE = new CorePackage();
    
  private CorePackage()
  {
    super(Symbol.of(NAME), null);
  }
  
  {
    // populate the package
    
    if (spartan.Config.LOG_DEBUG)
      log.info(() -> String.format("initializing package %s", NAME));
    
    bind(Symbol.of("="), CoreLib.EQ);
    bind(Symbol.of("/="), CoreLib.NE);
    bind(Symbol.of("<"), CoreLib.LT);
    bind(Symbol.of(">"), CoreLib.GT);
    bind(Symbol.of("<="), CoreLib.LE);
    bind(Symbol.of(">="), CoreLib.GE);
    bind(Symbol.of("not"), CoreLib.NOT);    
    bind(Symbol.of("apply"), CoreLib.APPLY);
    bind(Symbol.of("call/cc"), CoreLib.CALL_CC);
    bind(Symbol.of("print"), CoreLib.PRINT);
    bind(Symbol.of("print-line"), CoreLib.PRINT_LINE);
    bind(Symbol.of("type"), CoreLib.TYPE);    
    bind(Symbol.of("load"), CoreLib.LOAD);    
    bind(Symbol.of("gensym"), CoreLib.GENSYM);    
    bind(Symbol.of("abort"), CoreLib.ABORT);
    bind(Symbol.of("macroexpand-1"), CoreLib.MACROEXPAND1);
    bind(Symbol.of("identity-hash"), CoreLib.IDENTITY_HASH);
    bind(Symbol.of("identical?"), CoreLib.IS_IDENTICAL);
    bind(Symbol.of("current-time-ms"), CoreLib.CURRENT_TIME_MS);
    
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
    bind(Symbol.of("record?"), CoreLib.IS_RECORD);
    
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
    bind(Symbol.of("string->bytes"), CoreLib.STRING_TO_BYTES); // encode
    bind(Symbol.of("bytes->string"), CoreLib.BYTES_TO_STRING); // decode
    //bind(Symbol.of("string->number"), CoreLib.TEXT_TO_NUMBER);
    bind(Symbol.of("string->int"), CoreLib.TEXT_TO_INT);
    bind(Symbol.of("format-int"), CoreLib.FORMAT_INT);
    bind(Symbol.of("format-decimal"), CoreLib.FORMAT_DECIMAL);
    
    /* List procedures */
        
    bind(Symbol.of("adjoin"), ListLib.ADJOIN);
    bind(Symbol.of("first"), ListLib.FIRST);
    bind(Symbol.of("second"), ListLib.SECOND);
    bind(Symbol.of("third"), ListLib.THIRD);
    //bind(Symbol.of("fourth"), ListLib.FOURTH);
    bind(Symbol.of("rest"), ListLib.REST);
    bind(Symbol.of("empty?"), ListLib.IS_EMPTY);
    bind(Symbol.of("set-first!"), ListLib.SET_FIRST);
    bind(Symbol.of("set-rest!"), ListLib.SET_REST);
    bind(Symbol.of("list"), ListLib.MAKE_LIST);
    bind(Symbol.of("length"), ListLib.LENGTH);
    bind(Symbol.of("reverse"), ListLib.REVERSE);
    bind(Symbol.of("concat"), ListLib.CONCAT);
    bind(Symbol.of("append"), ListLib.APPEND);
    //bind(Symbol.of("take"), ListLib.TAKE);
    //bind(Symbol.of("drop"), ListLib.DROP);
    //bind(Symbol.of("nth"), ListLib.NTH);
    //bind(Symbol.of("nth-rest"), ListLib.NTH_REST);
    
    /* Vector procedures */
    
    bind(Symbol.of("vector"), VectorLib.FROM_LIST);
    bind(Symbol.of("make-vector"), VectorLib.MAKE);
    bind(Symbol.of("vector-ref"), VectorLib.REF);
    bind(Symbol.of("vector-set!"), VectorLib.SET);
    bind(Symbol.of("vector-length"), VectorLib.LENGTH);
    bind(Symbol.of("vector-copy"), VectorLib.COPY);
    bind(Symbol.of("vector-append!"), VectorLib.APPEND);
    bind(Symbol.of("vector-insert!"), VectorLib.INSERT);
    bind(Symbol.of("vector-remove!"), VectorLib.REMOVE);
    
    /* String & String Cursor procedures */
    
    bind(Symbol.of("string"), StringLib.FROM_LIST);
    bind(Symbol.of("string-ref"), StringLib.REF);
    bind(Symbol.of("string-length"), StringLib.LENGTH);
    bind(Symbol.of("string-empty?"), StringLib.IS_EMPTY);
    bind(Symbol.of("string-substring"), StringLib.SUBSTR);
    bind(Symbol.of("string-concat"), StringLib.CONCAT);
    bind(Symbol.of("string-join"), StringLib.JOIN);
    bind(Symbol.of("string-split"), StringLib.SPLIT);    
    bind(Symbol.of("string-reverse"), StringLib.REVERSE);
    bind(Symbol.of("string-find"), StringLib.FIND);
    bind(Symbol.of("string-replace"), StringLib.REPLACE);
    bind(Symbol.of("string-insert"), StringLib.INSERT);
    bind(Symbol.of("string-delete"), StringLib.DELETE);
    bind(Symbol.of("string-hash"), StringLib.HASH);
    bind(Symbol.of("string-cursor-begin"), StringLib.CURSOR_BEGIN);
    bind(Symbol.of("string-cursor-end"), StringLib.CURSOR_END);
    bind(Symbol.of("string-cursor-next"), StringLib.CURSOR_NEXT);
    bind(Symbol.of("string-cursor-prev"), StringLib.CURSOR_PREV);
        
    /* I/O and Port procedures */
    
    bind(Symbol.of("*standard-input-port*"), InputPort.STDIN);
    bind(Symbol.of("*standard-output-port*"), OutputPort.STDOUT);
    bind(Symbol.of("*standard-error-port*"), OutputPort.STDERR);
    bind(Symbol.of("port-open-file"), PortLib.OPEN);
    bind(Symbol.of("port-close"), PortLib.CLOSE);
    bind(Symbol.of("port-read"), PortLib.READ);
    bind(Symbol.of("port-write"), PortLib.WRITE);    
    bind(Symbol.of("port-open?"), PortLib.IS_OPEN);
    bind(Symbol.of("port-position"), PortLib.POSITION);
    bind(Symbol.of("port-seek"), PortLib.SEEK);
    bind(Symbol.of("port-size"), PortLib.SIZE);
    
    /* Bytevector related procedures */
    
    bind(Symbol.of("make-bytes"), BytesLib.MAKE);
    bind(Symbol.of("bytes-ref"), BytesLib.REF);
    bind(Symbol.of("bytes-set!"), BytesLib.SET);
    bind(Symbol.of("bytes-length"), BytesLib.LENGTH);
    //bind(Symbol.of("bytes-write-int32"), BytesLib.WRITE_INT32);
    
    /* Record related procedures */
    
    bind(Symbol.of("make-record-type"), CoreLib.MAKE_RECORD_TYPE);
    bind(Symbol.of("record-constructor"), CoreLib.RECORD_CONSTRUCTOR);
    bind(Symbol.of("record-predicate"), CoreLib.RECORD_PREDICATE);
    bind(Symbol.of("record-accessor"), CoreLib.RECORD_ACCESSOR);
    bind(Symbol.of("record-mutator"), CoreLib.RECORD_MUTATOR);    
    bind(Symbol.of("record-descriptor"), CoreLib.GET_DESCRIPTOR);
    
    /* Symbol related procedures */
    
    bind(Symbol.of("symbol-intern"), CoreLib.SYMBOL_INTERN);
    bind(Symbol.of("make-symbol"), CoreLib.MAKE_SYMBOL);
    bind(Symbol.of("symbol-package"), CoreLib.SYMBOL_PACKAGE);
    bind(Symbol.of("symbol-basename"), CoreLib.SYMBOL_BASENAME);
    bind(Symbol.of("symbol-qualified?"), CoreLib.SYMBOL_IS_QUALIFIED);
    
    /* Package-related procedures */
    
    bind(Symbol.of("current-package"), PackageLib.CURRENT_PACKAGE);
    bind(Symbol.of("set-current-package!"), PackageLib.SET_CURRENT_PACKAGE);
    bind(Symbol.of("make-package"), PackageLib.MAKE);
    bind(Symbol.of("find-package"), PackageLib.FIND);
    bind(Symbol.of("the-package"), PackageLib.GET);
    bind(Symbol.of("package-symbols"), PackageLib.SYMBOLS);
    bind(Symbol.of("package-alias"), PackageLib.ALIAS);
    bind(Symbol.of("package-bind"), PackageLib.BIND);
    bind(Symbol.of("package-resolve"), PackageLib.RESOLVE);
    
  }
}
