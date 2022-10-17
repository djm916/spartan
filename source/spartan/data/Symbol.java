package spartan.data;

public final class Symbol extends Datum
{
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
  
  public static Symbol generateUnique()
  {
    return new Symbol(String.format("#%d", nextUniqueId++));
  }
  
  @Override
  public Type type()
  {
    return Type.SYMBOL;
  }
  
  @Override
  public String repr()
  {
    return id;
  }
  
  public Symbol(String id)
  {
    this.id = id;
  }
  
  public boolean eq(Symbol that)
  {
    return this.id.equals(that.id);
  }
  
  @Override // for use as HashMap key
  public boolean equals(Object that)
  {
    return (that instanceof Symbol s) && id.equals(s.id);
  }
  
  @Override // for use as HashMap key
  public int hashCode()
  {
    return id.hashCode();
  }
  
  private static int nextUniqueId;
  private final String id;
}
