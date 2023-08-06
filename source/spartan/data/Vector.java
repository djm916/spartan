package spartan.data;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.stream.Collectors;
import spartan.errors.IndexOutOfBounds;
import spartan.errors.InvalidArgument;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.builtins.Core;

public final class Vector implements Datum, Iterable<Datum>
{
  public static Vector fromList(List elems)
  {
    var result = new Vector(elems.length());
    for (; !elems.empty(); elems = elems.cdr())
      result.append(elems.car());
    return result;
  }
    
  public Vector(int initialCapacity)
  {
    try {
      this.elems = new ArrayList<>(initialCapacity);
    }
    catch (IllegalArgumentException ex) {
      throw new InvalidArgument();
    }
  }
  
  public Vector(int length, Datum init)
  {
    this(length);
    for (int i = 0; i < length; ++i)
      elems.add(init);
  }

  public Vector()
  {
    this(DEFAULT_INITIAL_CAPACITY);
  }
  
  public Vector(Vector that)
  { 
    this(that.length());
    elems.addAll(that.elems);
  }
    
  @Override // Datum
  public String type()
  {
    return "vector";
  }
  
  @Override // Datum
  public String repr()
  {
    return elems.stream()
      .map(e -> e.repr())
      .collect(Collectors.joining(" ", "[", "]"));
  }  
    
  public int length()
  {
    return elems.size();
  }
  
  public Datum ref(int index)
  {
    try {
      return elems.get(index);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
    
  public void set(int index, Datum value)
  {
    try {
      elems.set(index, value);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
    
  public void append(Datum e)
  {
    elems.add(e);
  }
  
  public void insert(int i, Datum e)
  {
    try {
      elems.add(i, e);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void remove(int i)
  {
    try {
      elems.remove(i);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void fill(Datum x)
  {
    for (int i = 0; i < elems.size(); ++i)
      elems.set(i, x);
  }
  
  @Override // Iterable
  public Iterator<Datum> iterator()
  {
    return elems.iterator();
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
    
  private static final int DEFAULT_INITIAL_CAPACITY = 8;
  private final ArrayList<Datum> elems;
}
