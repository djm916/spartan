package spartan.data;

import spartan.util.WeakCache;

public sealed class Symbol implements Datum, IEq
permits QualifiedSymbol
{
  private static WeakCache<String, Symbol> cache = new WeakCache<>();
  private static int nextUniqueId;
  private final String name;    // full (qualified or unqualified) print name of the symbol  
  
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
  public static final Symbol CAR = new Symbol("car");
  public static final Symbol CDR = new Symbol("cdr");
  public static final Symbol Q_CAR = new QualifiedSymbol("spartan.core", "car");
  public static final Symbol Q_CDR = new QualifiedSymbol("spartan.core", "cdr");
  public static final Symbol Q_CONS = new QualifiedSymbol("spartan.core", "cons");
  public static final Symbol Q_CONCAT = new QualifiedSymbol("spartan.core", "list-concat");
  public static final Symbol MATCH = new Symbol("match");
  public static final Symbol UNDERSCORE = new Symbol("_");
  public static final Symbol LIST = new Symbol("list");
  public static final Symbol LIST_STAR = new Symbol("list*");
  public static final Symbol VECTOR = new Symbol("vector");
  public static final Symbol RECORD = new Symbol("record");
  
  /**
   * Returns an interned symbol with the given name.
   */
  public static Symbol of(String name)
  {
    return cache.get(name, () -> new Symbol(name));
  }
  
  /**
   * Generates a new, unique, uninterned symbol.
   */
  public static Symbol generateUnique()
  {
    return new Symbol(String.format("#%d", nextUniqueId++));
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.SYMBOL;
  }
  
  @Override // Datum
  public String repr()
  {
    return name();
  }
  
  public String name()
  {
    return name;
  }
  
  @Override // Object
  public boolean equals(Object rhs)
  {
    return (rhs instanceof Symbol s) && name.equals(s.name);
  }
  
  @Override // Object
  public int hashCode()
  {
    return name.hashCode();
  }
  
  public boolean equals(String rhs)
  {
    return name.equals(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Symbol rhs)
  {
    return this == rhs;
  }
    
  /**
   * Create a new, uninterned symbol for the given identifier
   */
  public Symbol(String name)
  {
    this.name = name;
  }
  
  public boolean isQualified()
  {
    return false;
  }
  
  public boolean isKeyword()
  {
    return name.charAt(0) == ':';
  }
  
  public boolean isSimple()
  {
    return !isQualified() && !isKeyword();
  }
  
  /**
   * Return an interned symbol. May return a new symbol or this (if previously interned)
   */
  public Symbol intern()
  {
    return cache.get(this.name, () -> this);
  }
}
