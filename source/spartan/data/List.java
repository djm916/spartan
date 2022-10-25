package spartan.data;

import java.util.function.Predicate;
import java.util.function.BiPredicate;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.stream.Collectors;
import spartan.errors.TypeMismatch;

public final class List implements Datum, Iterable<Datum>
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
    
    public Builder add(Datum... xs)
    {
      for (var x : xs)
        add(x);
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
    return new Builder().add(elems).build();
  }
  
  /**
   * Concatenates the given lists
   *
   * This is a not a member function in order to avoid a "Shlemiel The Painter" algorithm.
   */
  public static List concat(List lists)
  {
    var builder = new List.Builder();
    for (var list : lists) {
      if (list.type() != Type.LIST)
        throw new TypeMismatch();
      for (var elem : (List) list)
        builder.add(elem);
    }
    return builder.build();
  }
  
  @Override
  public Type type()
  {
    return Type.LIST;
  }
  
  @Override
  public String repr()
  {
    return stream()
      .map(e -> e.repr())
      .collect(Collectors.joining(" ", "(", ")"));
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
  
  public List removeInplace(Predicate<Datum> pred)
  {
    return removeInplace(this, pred);
  }
    
  public int indexOf(Predicate<Datum> pred)
  {
    return indexOf(this, pred);
  }
  
  @Override // Iterable
  public Iterator<Datum> iterator()
  {
    return new Iterator<>() {
      private List cur = List.this;
      
      @Override
      public boolean hasNext() {
        return cur != EMPTY;
      }
      
      @Override
      public Datum next() {
        if (cur == EMPTY)
          throw new NoSuchElementException();
        var val = cur.first;
        cur = cur.rest;
        return val;
      }
    };
  }
  
  @Override // Iterable
  public Spliterator<Datum> spliterator()
  {
    return Spliterators.spliteratorUnknownSize(iterator(), Spliterator.ORDERED | Spliterator.NONNULL);
  }
  
  public Stream<Datum> stream()
  {
    return StreamSupport.stream(spliterator(), false);
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
  
  private static List removeInplace(List list, Predicate<Datum> pred)
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
  
  private static int indexOf(List list, Predicate<Datum> pred)
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
