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
    return String.format("(%s)", repr(this));
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
    return length(this);
  }
  
  public Datum at(int index)
  {
    return at(this, index);
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
  
  public void forEach(Consumer<Datum> f)
  {
    for (var list = this; list != EMPTY; list = list.rest)
      f.accept(list.first);
  }
  
  public int indexOf(Datum x, Predicate<Datum> pred)
  {
    return indexOf(this, x, pred);
  }
  
  private static int length(List list)
  {
    int length = 0;
    for (; list != EMPTY; list = list.rest)
      length += 1;
    return length;
  }
  
  private static Datum at(List list, int index)
  {
    while (index-- > 0)
      list = list.rest;
    return list.first;
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
  
  private static List remove(List self, Predicate<Datum> pred)
  {
    var result = new Builder();
    for (; self != EMPTY; self = self.rest)
      if (!pred.test(self.first))
        result.add(self.first);
    return result.build();
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
  
  private static String repr(List self)
  {
    if (self == EMPTY)
      return "";
    
    if (self.rest == EMPTY)
      return self.first.repr();
    
    return String.format("%s %s", self.first.repr(), repr(self.rest));
  }
  
  private List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
    
  private Datum first;
  private List rest;  
}
