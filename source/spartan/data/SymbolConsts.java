package spartan.data;

public interface SymbolConsts
{
  /*
     Define symbols for each of the special forms recognized by the compiler.
  */
  public static final Symbol DEF = new Symbol("def");
  public static final Symbol DEFUN = new Symbol("defun");
  public static final Symbol DEFMACRO = new Symbol("defmacro");
  public static final Symbol DEFMETHOD = new Symbol("defmethod");
  public static final Symbol FUN = new Symbol("fun");
  public static final Symbol IF = new Symbol("if");
  public static final Symbol COND = new Symbol("cond");
  public static final Symbol ELSE = new Symbol("else");
  public static final Symbol QUOTE = new Symbol("quote");
  public static final Symbol QUASIQUOTE = new Symbol("quasiquote");
  public static final Symbol UNQUOTE = new Symbol("unquote");  
  public static final Symbol UNQUOTE_SPLICING = new Symbol("unquote-splicing");
  public static final Symbol LET = new Symbol("let");
  public static final Symbol LETSTAR = new Symbol("let*");
  public static final Symbol LETREC = new Symbol("letrec");
  public static final Symbol WHILE = new Symbol("while");
  public static final Symbol DO = new Symbol("do");
  public static final Symbol FOR = new Symbol("for");
  public static final Symbol SET = new Symbol("set!");
  public static final Symbol AND = new Symbol("and");
  public static final Symbol OR = new Symbol("or");
  public static final Symbol AMPERSAND = new Symbol("&");
  public static final Symbol MATCH = new Symbol("match");
  public static final Symbol UNDERSCORE = new Symbol("_");
  public static final Symbol LIST = new Symbol("list");
  public static final Symbol LIST_STAR = new Symbol("list*");
  public static final Symbol VECTOR = new Symbol("vector");
  public static final Symbol RECORD = new Symbol("record");
  public static final QualifiedSymbol ADJOIN = new QualifiedSymbol("spartan.base", "adjoin");
  public static final QualifiedSymbol CONCAT = new QualifiedSymbol("spartan.base", "concat");
}
