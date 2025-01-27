package spartan.data;

import java.util.function.Predicate;
import java.util.function.BiPredicate;
import java.util.function.UnaryOperator;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.function.Supplier;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;

final class EmptyList extends List
{
  public static final EmptyList VALUE = new EmptyList();
  
  @Override // Datum
  public String repr()
  {
    return "()";
  }
  
  @Override // Datum
  public boolean boolValue()
  {
    return false;
  }
  
  @Override // List
  public boolean isEmpty()
  {
    return true;
  }
  
  @Override // List
  public Datum first()
  {
    throw new NoSuchElement();
  }
  
  @Override // List
  public List rest()
  {
    throw new NoSuchElement();
  }
  
  private EmptyList()
  {
    super(null, null);
  }
}

public sealed class List implements Datum, Iterable<Datum>
permits EmptyList
{
  public static final List EMPTY = EmptyList.VALUE;
  
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
    
    public Builder addAll(Builder other)
    {
      if (head == EMPTY)
        return other;
      else {
        tail = tail.rest = other.head;
      }
      return this;
    }
    
    public List build()
    {
      return head;
    }
  }
  
  public static final Collector<Datum, Builder, List> COLLECTOR =
    Collector.of(Builder::new, Builder::add, Builder::addAll, Builder::build);
  
  public static List adjoin(Datum first, List rest)
  {
    return new List(first, rest);
  }
  
  public static List of(Datum... elems)
  {
    return new Builder().add(elems).build();
  }
  
  public static <E extends Datum> List of(Iterable<E> elems)
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
  public static <E extends Datum> List concat(Iterable<E> lists)
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
  
  public static List concat2(List left, List right)
  {
    var builder = new List.Builder();
    for (var elem : left)
      builder.add(elem);
    for (var elem : right)
      builder.add(elem);
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
  
  public Datum first()
  {
    return first;
  }
  
  public void setFirst(Datum first)
  {
    this.first = first;
  }
  
  public List rest()
  {
    return rest;
  }
  
  public void setRest(List rest)
  {
    this.rest = rest;
  }
  
  public Datum second()
  {
    return rest().first();
  }
  
  public Datum third()
  {    
    return rest().rest().first();
  }
  
  public Datum fourth()
  {
    return rest().rest().rest().first();
  }
  
  public List drop2()
  {
    return rest().rest();
  }
  
  public List drop3()
  {
    return rest().rest().rest();
  }
  
  public Datum drop(int n)
  {
    return drop(this, n);
  }
  
  public Datum take(int n)
  {
    return drop(this, n);
  }
  
  public Datum get(int index)
  {
    return get(this, index);
  }
  
  public int length()
  {
    return length(this);
  }
  
  public boolean isEmpty()
  {
    return false;
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
      
  public int index(Predicate<Datum> pred)
  {
    return index(this, pred);
  }
    
  public List map(UnaryOperator<Datum> f)
  {
    return map(this, f);
  }
  
  public boolean equals(Object other)
  {
    return other instanceof List rhs && isEqual(this, rhs, (a, b) -> a.equals(b));
  }
  
  public boolean isEqual(List rhs)
  {
    return isEqual(this, rhs, (a, b) -> (a instanceof IEq x) && (b instanceof IEq y) && x.isEqual(y));
  }
  
  @Override // Iterable
  public Iterator<Datum> iterator()
  {
    return new Iterator<>() {
      private List cur = List.this;
      
      @Override
      public boolean hasNext() {
        return !cur.isEmpty();
      }
      
      @Override
      public Datum next() {
        var val = cur.first();
        cur = cur.rest();
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
  
  public <T extends Datum> Stream<T> streamOf(Class<T> clazz)
  {
    return stream().map(clazz::cast);
  }
  
  public <T extends Datum> Stream<T> streamOf(Class<T> clazz, Supplier<Error> onError)
  {
    return stream().map(e -> { try { return clazz.cast(e); } catch (ClassCastException ex) { throw onError.get(); } });
  }
  
  public Datum[] toArray()
  {
    return toArray(this);
  }
  
  private static Datum[] toArray(List list)
  {
    var result = new Datum[list.length()];
    for (int i = 0; !list.isEmpty(); ++i, list = list.rest())
      result[i] = list.first();
    return result;
  }
  
  private static int length(List list)
  {
    int length = 0;
    for (; !list.isEmpty(); list = list.rest())
      ++length;
    return length;
  }
  
  private static Datum get(List list, int index)
  {
    for (; !list.isEmpty() && index > 0; list = list.rest(), --index)
      ;
    return list.first();
  }
  
  private static Datum take(List list, int n)
  {
    var result = new Builder();
    for (; !list.isEmpty() && n > 0; list = list.rest(), --n)
      result.add(list.first());
    return result.build();
  }
  
  private static Datum drop(List list, int n)
  {
    for (; !list.isEmpty() && n > 0; list = list.rest(), --n)
      ;
    return list;
  }
  
  private static List append(List list, Datum x)
  {
    var result = new Builder();
    for (; !list.isEmpty(); list = list.rest())
      result.add(list.first());
    result.add(x);
    return result.build();
  }
  
  private static List reverse(List list)
  {
    var result = EMPTY;
    for (; !list.isEmpty(); list = list.rest())
      result = adjoin(list.first(), result);
    return result;
  }
  
  private static List remove(List list, Predicate<Datum> pred)
  {
    var result = new Builder();
    for (; !list.isEmpty(); list = list.rest())
      if (!pred.test(list.first()))
        result.add(list.first());
    return result.build();
  }
  
  private static int index(List list, Predicate<Datum> pred)
  {
    for (int i = 0; list != EMPTY; list = list.rest, ++i)
      if (pred.test(list.first))
        return i;
    return -1;
  }
  
  private static List map(List list, UnaryOperator<Datum> f)
  {
    var result = new List.Builder();
    for (; !list.isEmpty(); list = list.rest())
      result.add(f.apply(list.first));
    return result.build();
  }
  
  private static boolean isEqual(List lhs, List rhs, BiPredicate<Datum, Datum> equals)
  {
    for (; !lhs.isEmpty() && !rhs.isEmpty(); lhs = lhs.rest(), rhs = rhs.rest())
      if (!equals.test(lhs.first(), rhs.first()))
        return false;
    return lhs.isEmpty() && rhs.isEmpty();
  }
  
  protected List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  private Datum first;
  private List rest;  
}
