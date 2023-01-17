package spartan.data;

import java.util.function.Predicate;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.stream.Collectors;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.builtins.Core;

public final class List implements Datum, ISeq, ISize, IEq, Iterable<Datum>
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
  
  public static List of(Iterable<Datum> elems)
  {
    var builder = new List.Builder();
    for (var elem : elems)
      builder.add(elem);
    return builder.build();
  }
  
  /**
   * Concatenates the given lists
   *
   * This is a not a member function in order to avoid a "Shlemiel The Painter" O(N^2) algorithm.
   */
  public static List concat(List lists)
  {
    var builder = new List.Builder();
    for (var car : lists) {
      if (!(car instanceof List list))
        throw new TypeMismatch();
      for (var elem : list)
        builder.add(elem);
    }
    return builder.build();
  }
  
  @Override
  public String type()
  {
    return "list";
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
    if (empty())
      throw new NoSuchElement();
    return first;
  }
  
  public void setCar(Datum x)
  {
    if (empty())
      throw new NoSuchElement();
    first = x;
  }
  
  public List cdr()
  {
    if (empty())
      throw new NoSuchElement();
    return rest;
  }
  
  public void setCdr(List x)
  {
    if (empty())
      throw new NoSuchElement();
    rest = x;
  }
  
  public Datum cadr()
  {
    return cdr().car();
  }
  
  public Datum caddr()
  {    
    return cdr().cdr().car();
  }
  
  public Datum cadddr()
  {
    return cdr().cdr().cdr().car();
  }
  
  public List cddr()
  {
    return cdr().cdr();
  }
  
  public List cdddr()
  {
    return cdr().cdr().cdr();
  }
  
  @Override // ISize
  public int length()
  {
    int length = 0;
    for (var list = this; !list.empty(); list = list.cdr())
      ++length;
    return length;
  }
  
  @Override // IEq
  public boolean isEqual(List rhs)
  {
    return isEqual(this, rhs);
  }
  
  @Override // ISeq
  public Datum at(int index)
  {
    var list = this;
    for (; !list.empty() && index > 0; --index, list = list.cdr())
      ;
    return list.car();
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
        return !cur.empty();
      }
      
      @Override
      public Datum next() {
        var val = cur.car();
        cur = cur.cdr();
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
  
  private static List append(List list, Datum x)
  {
    var result = new Builder();
    for (; !list.empty(); list = list.cdr())
      result.add(list.car());
    result.add(x);
    return result.build();
  }
  
  private static List reverse(List list)
  {
    var result = List.EMPTY;
    for (; !list.empty(); list = list.cdr())
      result = cons(list.car(), result);
    return result;
  }
  
  private static List remove(List list, Predicate<Datum> pred)
  {
    var result = new Builder();
    for (; !list.empty(); list = list.cdr())
      if (!pred.test(list.car()))
        result.add(list.car());
    return result.build();
  }
  
  private static List removeInplace(List list, Predicate<Datum> pred)
  {
    var cur = list;
    // Remove leading elements
    while (cur != EMPTY && pred.test(cur.first))
      list = cur = cur.rest;
    if (cur == EMPTY)
      return EMPTY;
    var prev = cur;
    cur = cur.rest;
    while (cur != EMPTY) {
      if (pred.test(cur.first))
        cur = cur.rest;
      else {
        prev.rest = cur;
        prev = cur;
        cur = cur.rest;
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
  
  private static boolean isEqual(List lhs, List rhs)
  {
    for (; lhs != EMPTY && rhs != EMPTY; lhs = lhs.rest, rhs = rhs.rest)
      if (lhs.car() instanceof IEq x && rhs.car() instanceof IEq y)
        if (! x.isEqual(y))
          return false;    
    return lhs == EMPTY && rhs == EMPTY;
  }
  
  private List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  private Datum first;
  private List rest;  
}
