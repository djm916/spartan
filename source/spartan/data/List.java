package spartan.data;

import java.util.function.Predicate;
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

final class Null extends List
{
  public static final Null VALUE = new Null();
  
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
  public Datum car()
  {
    throw new NoSuchElement();
  }
  
  @Override // List
  public void setCar(Datum x)
  {
    throw new NoSuchElement();
  }
  
  @Override // List
  public List cdr()
  {
    throw new NoSuchElement();
  }
  
  @Override // List
  public void setCdr(List x)
  {
    throw new NoSuchElement();
  }
  
  private Null()
  {
    super(null, null);
  }
}

public sealed class List implements Datum, Iterable<Datum>
permits Null
{
  public static final List EMPTY = Null.VALUE;
  
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
  
  public static List cons(Datum first, List rest)
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
  
  public Datum get(int index)
  {
    return get(this, index);
  }
  
  public void set(int index, Datum value)
  {
    set(this, index, value);
  }
  
  public Datum tail(int index)
  {
    return tail(this, index);
  }
  
  public void setTail(int index, List value)
  {
    setTail(this, index, value);
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
    for (int i = 0; !list.isEmpty(); ++i, list = list.cdr())
      result[i] = list.car();
    return result;
  }
  
  private static int length(List list)
  {
    int length = 0;
    for (; !list.isEmpty(); list = list.cdr())
      ++length;
    return length;
  }
  
  private static Datum get(List list, int index)
  {
    for (; !list.isEmpty() && index > 0; list = list.cdr(), --index)
      ;
    return list.car();
  }
  
  private static void set(List list, int index, Datum value)
  {
    for (; !list.isEmpty() && index > 0; list = list.cdr(), --index)
      ;
    list.setCar(value);
  }
  
  private static Datum tail(List list, int index)
  {
    for (; !list.isEmpty() && index > 0; list = list.cdr(), --index)
      ;
    return list;
  }
  
  private static void setTail(List list, int index, List value)
  {
    for (; !list.isEmpty() && index > 0; list = list.cdr(), --index)
      ;
    list.setCdr(value);
  }
  
  private static List append(List list, Datum x)
  {
    var result = new Builder();
    for (; !list.isEmpty(); list = list.cdr())
      result.add(list.car());
    result.add(x);
    return result.build();
  }
  
  private static List reverse(List list)
  {
    var result = List.EMPTY;
    for (; !list.isEmpty(); list = list.cdr())
      result = cons(list.car(), result);
    return result;
  }
  
  private static List remove(List list, Predicate<Datum> pred)
  {
    var result = new Builder();
    for (; !list.isEmpty(); list = list.cdr())
      if (!pred.test(list.car()))
        result.add(list.car());
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
    for (; !list.isEmpty(); list = list.cdr())
      result.add(f.apply(list.first));
    return result.build();
  }
  
  protected List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  private Datum first;
  private List rest;  
}
