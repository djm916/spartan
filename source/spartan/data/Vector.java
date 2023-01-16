package spartan.data;

//import java.util.function.BiPredicate;
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

public final class Vector implements Datum, Callable, IEq<Vector>, ISize, Iterable<Datum>
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
  public Type type()
  {
    return Type.VECTOR;
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
    if (! (vm.peekArg() instanceof IInt))
      throw new InvalidArgument();
    int index = ((IInt)vm.popArg()).intValue();
    vm.result = get(index);
    vm.popFrame();
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
  
  public Datum get(int index)
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
  public boolean isEqual(Vector other)
  {
    if (this.length() != other.length())
      return false;
    
    var n = this.elems.size();
    for (int i = 0; i < n; ++i)
      if (! Core.isEqual(this.elems.get(i), other.elems.get(i)))
        return false;
    
    return true;
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
