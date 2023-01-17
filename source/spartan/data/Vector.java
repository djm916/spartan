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
import spartan.runtime.VirtualMachine;
import spartan.builtins.Core;

public final class Vector implements Datum, Callable, ISeq, ISize, IEq, Iterable<Datum>
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
    elems = new ArrayList<>(initialCapacity);
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
  
  @Override // Callable
  public void apply(VirtualMachine vm)
  {
    if (vm.popArg() instanceof IInt index) {
      vm.result = at(index.intValue());
      vm.popFrame();
    }
    else throw new InvalidArgument();
  }
  
  @Override // Callable
  public boolean arityMatches(int numArgs)
  {
    return numArgs == 1;
  }
  
  @Override // ISize
  public int length()
  {
    return elems.size();
  }
  
  @Override // ISeq
  public Datum at(int index)
  {
    try {
      return elems.get(index);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new IndexOutOfBounds();
    }
  }
  
  public void set(int index, Datum value)
  {
    try {
      elems.set(index, value);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new IndexOutOfBounds();
    }
  }
  
  public void append(Datum x)
  {
    elems.add(x);
  }
  
  public void fill(Datum x)
  {
    for (int i = 0; i < elems.size(); ++i)
      elems.set(i, x);
  }

  @Override // IEq
  public boolean isEqual(Vector rhs)
  {
    return isEqual(this, rhs);
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
  
  private static boolean isEqual(Vector lhs, Vector rhs)
  {
    if (lhs.length() != rhs.length())
      return false;    
    var n = lhs.length();
    for (int i = 0; i < n; ++i)
      if (lhs.elems.get(i) instanceof IEq x && rhs.elems.get(i) instanceof IEq y)
        if (! x.isEqual(y))
          return false;    
    return true;
  }
  
  private static final int DEFAULT_INITIAL_CAPACITY = 8;
  private final ArrayList<Datum> elems;
}
