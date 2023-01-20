package spartan.data;

public final class Symbol implements Datum, IEq   
{
  private static WeakCache<String, Symbol> cache = new WeakCache<>();
  private static int nextUniqueId;
  private final String id;
  
  public static final Symbol DEF = new Symbol("def");
  public static final Symbol DEFUN = new Symbol("defun");
  public static final Symbol DEFMACRO = new Symbol("defmacro");
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
  public static final Symbol DO = new Symbol("do");
  public static final Symbol WHILE = new Symbol("while");
  public static final Symbol FOR = new Symbol("for");
  public static final Symbol SET = new Symbol("set!");
  public static final Symbol AND = new Symbol("and");
  public static final Symbol OR = new Symbol("or");
  public static final Symbol AMPERSAND = new Symbol("&");
  public static final Symbol CALL_CC = new Symbol("call/cc");
    
  /**
   * Returns an interned symbol.
   */
  public static Symbol of(String id)
  {
    return cache.get(id, () -> new Symbol(id));
  }
  
  /**
   * Generates a new, unique, uninterned symbol.
   */
  public static Symbol generateUnique()
  {
    return new Symbol(String.format("#%d", nextUniqueId++));
  }
  
  @Override
  public String type()
  {
    return "symbol";
  }
  
  @Override
  public String repr()
  {
    return id;
  }
  
  @Override // Object
  public boolean equals(Object rhs)
  {
    return (rhs instanceof Symbol s) && id.equals(s.id);
  }
  
  public boolean equals(String rhs)
  {
    return id.equals(rhs);
  }
  
  @Override // Object
  public int hashCode()
  {
    return id.hashCode();
  }
  
  @Override // IEq
  public boolean isEqual(Symbol rhs)
  {
    return this == rhs;
  }
  
  /**
   * Create a new, uninterned symbol for the given identifier
   */
  public Symbol(String id)
  {
    this.id = id;
  }
  
  /**
   * Return an interned symbol. May return a new symbol or this (if previously interned)
   */
  public Symbol intern()
  {
    return cache.get(this.id, () -> this);
  }
}
