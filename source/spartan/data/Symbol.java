package spartan.data;

import java.util.Map;
import java.util.HashMap;

public final class Symbol extends Datum
{
  private static Map<String, Symbol> interned = new HashMap<>();
  
  public static final Symbol Fun = get("fun");
  public static final Symbol If = get("if");
  public static final Symbol Cond = get("cond");
  public static final Symbol Else = get("else");
  public static final Symbol Let = get("let");
  public static final Symbol LetStar = get("let*");
  public static final Symbol LetRec = get("letrec");
  public static final Symbol Do = get("do");
  public static final Symbol Def = get("def");
  public static final Symbol Defun = get("defun");
  public static final Symbol Defmacro = get("defmacro");
  public static final Symbol And = get("and");
  public static final Symbol Or = get("or");
  public static final Symbol Quote = get("quote");
  public static final Symbol Quasiquote = get("quasiquote");
  public static final Symbol Unquote = get("unquote");
  public static final Symbol UnquoteSplicing = get("unquote-splicing");  
  public static final Symbol Set = get("set!");
  public static final Symbol While = get("while");
  public static final Symbol For = get("for");
  public static final Symbol Ampersand = get("&");
  public static final Symbol Cons = get("cons");
  public static final Symbol Concat = get("concat");
  public static final Symbol CallCC = get("call/cc");
  
  public static Symbol get(String id)
  {
    if (!interned.containsKey(id))
      interned.put(id, new Symbol(id));
    return interned.get(id);
  }
  
  public static Symbol gen()
  {
    return get(String.format("#%d", nextSymbolNum++));
  }
  
  public Type type()
  {
    return Type.Symbol;
  }
  
  public String repr()
  {
    return id;
  }
    
  private Symbol(String id)
  {
    this.id = id;
  }
  
  
  private static int nextSymbolNum;
  
  private final String id;
}
