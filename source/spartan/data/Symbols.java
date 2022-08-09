package spartan.data;

/* Constants for commonly used symbols */

public final class Symbols
{
  public static final Symbol Fun = Symbol.get("fun");
  public static final Symbol If = Symbol.get("if");
  public static final Symbol Cond = Symbol.get("cond");
  public static final Symbol Let = Symbol.get("let");
  public static final Symbol LetStar = Symbol.get("let*");
  public static final Symbol LetRec = Symbol.get("letrec");
  public static final Symbol Def = Symbol.get("def");
  public static final Symbol Defun = Symbol.get("defun");
  public static final Symbol Defmacro = Symbol.get("defmacro");
  public static final Symbol And = Symbol.get("and");
  public static final Symbol Or = Symbol.get("or");
  public static final Symbol Quote = Symbol.get("quote");
  public static final Symbol Quasiquote = Symbol.get("quasiquote");
  public static final Symbol Unquote = Symbol.get("unquote");
  public static final Symbol UnquoteSplicing = Symbol.get("unquote-splicing");  
  public static final Symbol Do = Symbol.get("do");
  public static final Symbol Set = Symbol.get("set!");
  public static final Symbol While = Symbol.get("while");
  public static final Symbol Ampersand = Symbol.get("&");
  public static final Symbol Cons = Symbol.get("cons");
  public static final Symbol Concat = Symbol.get("concat");
  public static final Symbol CallCC = Symbol.get("call/cc");
  
  private Symbols() {}
}
