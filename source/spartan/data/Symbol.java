package spartan.data;

import java.util.Map;
import java.util.HashMap;

public final class Symbol extends Datum
{
  public static final Symbol Def = new Symbol("def");
  public static final Symbol Defun = new Symbol("defun");
  public static final Symbol Defmacro = new Symbol("defmacro");
  public static final Symbol Fun = new Symbol("fun");
  public static final Symbol If = new Symbol("if");
  public static final Symbol Cond = new Symbol("cond");
  public static final Symbol Else = new Symbol("else");  
  public static final Symbol Quote = new Symbol("quote");
  public static final Symbol Quasiquote = new Symbol("quasiquote");
  public static final Symbol Unquote = new Symbol("unquote");  
  public static final Symbol UnquoteSplicing = new Symbol("unquote-splicing");
  public static final Symbol Let = new Symbol("let");
  public static final Symbol LetStar = new Symbol("let*");
  public static final Symbol LetRec = new Symbol("letrec");
  public static final Symbol Do = new Symbol("do");
  public static final Symbol While = new Symbol("while");
  public static final Symbol For = new Symbol("for");
  public static final Symbol Set = new Symbol("set!");
  public static final Symbol And = new Symbol("and");
  public static final Symbol Or = new Symbol("or");
  public static final Symbol Ampersand = new Symbol("&");
  public static final Symbol CallCC = new Symbol("call/cc");

  public static Symbol generateUnique()
  {
    return new Symbol(String.format("#%d", nextSymbolNum++));
  }

  public Symbol(String id)
  {
    this.id = id;
  }
  
  public Type type()
  {
    return Type.SYMBOL;
  }
  
  public String repr()
  {
    return id;
  }
  
  public boolean eq(Symbol other)
  {
    return this.id.equals(other.id);
  }
    
  public boolean equals(Object other)
  {
    if (! (other instanceof Symbol))
      return false;
    return this.id.equals(((Symbol)other).id);
  }
  
  public int hashCode()
  {
    return id.hashCode();
  }
  
  private static int nextSymbolNum;
  private final String id;
}
