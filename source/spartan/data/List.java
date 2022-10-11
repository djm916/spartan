package spartan.data;

import java.util.function.Predicate;
import java.util.function.BiPredicate;
import java.util.function.Consumer;

public final class List extends Datum
{
  public static final List EMPTY = new List(null, null);
  
  public static class Builder
  {
    private List head = EMPTY, tail = EMPTY;
    
    public Builder add(Datum x)
    {
      if (head == EMPTY)
        head = tail = new List(x, EMPTY);
      else
        tail = tail.rest = new List(x, EMPTY);
      
      return this;
    }
    
    public List build()
    {
      return head;
    }
  }
  
  public static List cons(Datum first, List rest)
  {
    return new List(first, rest);
  }
  
  public static List of(Datum... elems)
  {
    var result = EMPTY;
    for (int i = elems.length - 1; i >= 0; --i)
      result = cons(elems[i], result);
    return result;
  }

  public Type type()
  {
    return Type.LIST;
  }
  
  public String repr()
  {
    var result = new StringBuilder();
    result.append("(");
    for (var list = this; list != List.EMPTY; list = list.cdr()) {
      result.append(list.car().repr());
      if (list.cdr() != EMPTY)
        result.append(" ");
    }
    result.append(")");
    return result.toString();
  }
  
  public boolean empty()
  {
    return this == EMPTY;
  }
  
  public Datum car()
  {
    return first;
  }
  
  public void setCar(Datum x)
  {
    first = x;
  }
  
  public List cdr()
  {
    return rest;
  }
  
  public void setCdr(List x)
  {
    rest = x;
  }
  
  public Datum cadr()
  {
    return rest.first;
  }
  
  public Datum caddr()
  {
    return rest.rest.first;
  }
  
  public Datum cadddr()
  {
    return rest.rest.rest.first;
  }
  
  public List cddr()
  {
    return rest.rest;
  }
  
  public List cdddr()
  {
    return rest.rest.rest;
  }
  
  public int length()
  {
    int length = 0;
    for (var list = this; list != EMPTY; list = list.rest)
      ++length;
    return length;
  }
  
  public Datum at(int index)
  {
    var list = this;
    while (index-- > 0)
      list = list.rest;
    return list.first;
  }
  
  public boolean eq(List other, BiPredicate<Datum, Datum> eq)
  {
    return eq(this, other, eq);
  }
  
  public List concat(List that)
  {
    return concat(this, that);
  }
  
  public List append(Datum x)
  {
    return append(this, x);
  }
  
  public List reverse()
  {
    return reverse(this);
  }
  
  public List remove(Predicate<Datum> pred)
  {
    return remove(this, pred);
  }
  
  public List removed(Predicate<Datum> pred)
  {
    return removed(this, pred);
  }
  
  public void forEach(Consumer<Datum> f)
  {
    for (var list = this; list != EMPTY; list = list.rest)
      f.accept(list.first);
  }
  
  public int indexOf(Datum x, Predicate<Datum> pred)
  {
    return indexOf(this, x, pred);
  }
    
  private static List concat(List x, List y)
  {
    var result = new Builder();
    for (; x != EMPTY; x = x.rest)
      result.add(x.first);
    for (; y != EMPTY; y = y.rest)
      result.add(y.first);
    return result.build();
  }
  
  private static List append(List x, Datum y)
  {
    var result = new Builder();
    for (; x != EMPTY; x = x.rest)
      result.add(x.first);
    result.add(y);
    return result.build();
  }
  
  private static List reverse(List x)
  {
    var result = List.EMPTY;
    for (; x != EMPTY; x = x.rest)
      result = cons(x.car(), result);
    return result;
  }
  
  private static List removed(List self, Predicate<Datum> pred)
  {
    var result = new Builder();
    for (; self != EMPTY; self = self.rest)
      if (!pred.test(self.first))
        result.add(self.first);
    return result.build();
  }
  
  private static List remove(List list, Predicate<Datum> pred)
  {
    List prev = null;
    List cur = list;
    for (; cur != EMPTY; prev = cur, cur = cur.cdr()) {
      if (pred.test(cur.first)) {
        if (prev == null)
          list = list.rest;
        else
          prev.rest = cur.rest;
        break;
      }
    }
    return list;
  }
  
  private static int indexOf(List list, Datum x, Predicate<Datum> pred)
  {
    for (int i = 0; list != EMPTY; list = list.rest, ++i)
      if (pred.test(list.first))
        return i;
    return -1;
  }
  
  private static boolean eq(List x, List y, BiPredicate<Datum, Datum> eq)
  {
    for (; x != EMPTY && y != EMPTY; x = x.rest, y = y.rest)
      if (!eq.test(x.first, y.first))
        return false;
    
    return x == EMPTY && y == EMPTY;
  }
    
  private List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
    
  private Datum first;
  private List rest;  
}
