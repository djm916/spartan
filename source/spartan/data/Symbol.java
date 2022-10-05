package spartan.data;

import java.util.Map;
import java.util.HashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

public final class Symbol extends Datum
{
  private static final Map<String, WeakReference<Symbol>> interned = new HashMap<>();
  private static final ReferenceQueue unused = new ReferenceQueue();
  
  public static final Symbol DEF = Symbol.of("def");
  public static final Symbol DEFUN = Symbol.of("defun");
  public static final Symbol DEFMACRO = Symbol.of("defmacro");
  public static final Symbol FUN = Symbol.of("fun");
  public static final Symbol IF = Symbol.of("if");
  public static final Symbol COND = Symbol.of("cond");
  public static final Symbol ELSE = Symbol.of("else");  
  public static final Symbol QUOTE = Symbol.of("quote");
  public static final Symbol QUASIQUOTE = Symbol.of("quasiquote");
  public static final Symbol UNQUOTE = Symbol.of("unquote");  
  public static final Symbol UNQUOTE_SPLICING = Symbol.of("unquote-splicing");
  public static final Symbol LET = Symbol.of("let");
  public static final Symbol LETSTAR = Symbol.of("let*");
  public static final Symbol LETREC = Symbol.of("letrec");
  public static final Symbol DO = Symbol.of("do");
  public static final Symbol WHILE = Symbol.of("while");
  public static final Symbol FOR = Symbol.of("for");
  public static final Symbol SET = Symbol.of("set!");
  public static final Symbol AND = Symbol.of("and");
  public static final Symbol OR = Symbol.of("or");
  public static final Symbol AMPERSAND = Symbol.of("&");
  public static final Symbol CALL_CC = Symbol.of("call/cc");

  public static Symbol of(String id)
  {
    var result = interned.get(id);
    if (result == null || result.get() == null) {
      var symbol = new Symbol(id);
      interned.put(id, new WeakReference<>(symbol, unused));
      purgeUnusedSymbols();
      return symbol;
    }
    return result.get();
  }
  
  public static Symbol generateUnique()
  {
    return Symbol.of(String.format("#%d", nextUniqueId++));
  }
  
  public Type type()
  {
    return Type.SYMBOL;
  }
  
  public String repr()
  {
    return id;
  }
  
  private Symbol(String id)
  {
    this.id = id;
  }
  
  private static void purgeUnusedSymbols()
  {
    //var beforeSize = interned.size();
    var used = interned.values();
    Reference<Symbol> ref = null;
    while ((ref = unused.poll()) != null)
      used.remove(ref);
    //var afterSize = interned.size();
    //System.out.println(String.format("purged symbol table (before: %d, after: %d)", beforeSize, afterSize));
  }
  
  private static int nextUniqueId;
  private final String id;
}
