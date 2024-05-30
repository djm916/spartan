package spartan.builtins;

import spartan.data.*;
import java.util.Optional;
import java.util.logging.Logger;

public final class CorePackage extends spartan.data.Package
{
  private static final Logger log = Logger.getLogger(CorePackage.class.getName());
  
  public static final String NAME = "spartan.core";
  
  public static final CorePackage INSTANCE = new CorePackage();
  
  /*
  public static CorePackage getInstance()
  {
    if (instance == null)
      instance = new CorePackage();
    return instance;
  }
  */
  
  private CorePackage()
  {
    super(Symbol.of(NAME), spartan.data.Package.NONE);
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
    bind(Symbol.of("nil"), Nil.VALUE);
    bind(Symbol.of("true"), Bool.TRUE);
    bind(Symbol.of("false"), Bool.FALSE);    
    bind(Symbol.of("not"), CoreLib.NOT);    
    bind(Symbol.of("apply"), CoreLib.APPLY);
    bind(Symbol.of("call/cc"), CoreLib.CALL_CC);
    bind(Symbol.of("print"), CoreLib.PRINT);
    bind(Symbol.of("print-line"), CoreLib.PRINT_LINE);
    bind(Symbol.of("type"), CoreLib.TYPE);    
    bind(Symbol.of("load"), CoreLib.LOAD);    
    bind(Symbol.of("gensym"), CoreLib.GENSYM);
    bind(Symbol.of("identity-hash"), CoreLib.IDENTITY_HASH);
    bind(Symbol.of("error"), CoreLib.ERROR);
    bind(Symbol.of("at"), CoreLib.AT);
    bind(Symbol.of("set-at!"), CoreLib.SET_AT);
    bind(Symbol.of("length"), CoreLib.LENGTH);
    bind(Symbol.of("empty?"), CoreLib.IS_EMPTY);
    bind(Symbol.of("macroexpand-1"), CoreLib.MACROEXPAND1);
    bind(Symbol.of("print-traceback"), CoreLib.PRINT_TRACEBACK);
    bind(Symbol.of("*package*"), this);
    
    /* Package-related procedures */
    
    bind(Symbol.of("make-package"), PackageLib.MAKE);
    bind(Symbol.of("find-package"), PackageLib.FIND);
    bind(Symbol.of("package-exists?"), PackageLib.EXISTS);
    bind(Symbol.of("package-symbols"), PackageLib.SYMBOLS);
    bind(Symbol.of("package-add-alias"), PackageLib.ADD_ALIAS);
    //bind(Symbol.of("package-bound?"), PackageLib.BOUND);
    bind(Symbol.of("package-bind"), PackageLib.BIND);
    bind(Symbol.of("package-resolve"), PackageLib.RESOLVE);
    
    //bind(Symbol.of("print-package"), CoreLib.PRINT_PACKAGE);
    
    /* Symbol related procedures */
    
    bind(Symbol.of("symbol-intern"), CoreLib.SYMBOL_INTERN);
    //bind(Symbol.of("make-symbol"), CoreLib.MAKE_SYMBOL);
    //bind(Symbol.of("symbol-package"), CoreLib.SYMBOL_PACKAGE);
    //bind(Symbol.of("symbol-name"), CoreLib.SYMBOL_NAME);
    //bind(Symbol.of("symbol-qualified?"), CoreLib.SYMBOL_QUALIFIED);
    
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
    bind(Symbol.of("table?"), CoreLib.IS_TABLE);
    
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
    bind(Symbol.of("set-car!"), ListLib.SET_CAR);
    bind(Symbol.of("set-cdr!"), ListLib.SET_CDR);
            
    /* Vector procedures */
    
    bind(Symbol.of("vector"), VectorLib.FROM_LIST);
    bind(Symbol.of("make-vector"), VectorLib.MAKE);
    bind(Symbol.of("vector-copy"), VectorLib.COPY);
    bind(Symbol.of("vector-append!"), VectorLib.APPEND);
    bind(Symbol.of("vector-insert!"), VectorLib.INSERT);
    bind(Symbol.of("vector-remove!"), VectorLib.REMOVE);
    bind(Symbol.of("vector-length"), VectorLib.LENGTH);
    
    /* String procedures */
    
    bind(Symbol.of("string-concat"), StringLib.CONCAT);
    bind(Symbol.of("string-join"), StringLib.JOIN);
    bind(Symbol.of("string-substr"), StringLib.SUBSTR);
    bind(Symbol.of("string-reverse"), StringLib.REVERSE);
    bind(Symbol.of("string-hash"), StringLib.HASH);
    bind(Symbol.of("string-length"), StringLib.LENGTH);
    
    /* I/O and Port procedures */
    
    bind(Symbol.of("standard-input-port"), InputPort.STDIN);
    bind(Symbol.of("standard-output-port"), OutputPort.STDOUT);
    bind(Symbol.of("port-open-file"), PortLib.OPEN);
    bind(Symbol.of("port-close"), PortLib.CLOSE);
    bind(Symbol.of("port-read"), PortLib.READ);
    bind(Symbol.of("port-write"), PortLib.WRITE);    
    bind(Symbol.of("port-open?"), PortLib.IS_OPEN);
    bind(Symbol.of("port-position"), PortLib.POSITION);
    bind(Symbol.of("port-seek"), PortLib.SEEK);
    bind(Symbol.of("port-size"), PortLib.SIZE);
    
    /* Table procedures */
    
    bind(Symbol.of("table"), TableLib.FROM_LIST);
    bind(Symbol.of("table-contains?"), TableLib.CONTAINS);
    bind(Symbol.of("table-keys"), TableLib.KEYS);
    bind(Symbol.of("table-values"), TableLib.VALUES);
    
    /* Structure related procedures */
    
    bind(Symbol.of("register-struct-type"), CoreLib.MAKE_STRUCT_TYPE);
    bind(Symbol.of("make-struct"), CoreLib.MAKE_STRUCT);
    
    //bind(Symbol.of("repr"), CoreLib.GENERIC_REPR);
    //bind(Symbol.of("at"), CoreLib.GENERIC_AT);
  }
}
