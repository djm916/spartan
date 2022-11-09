package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.function.BiPredicate;
import java.util.ArrayList;
import spartan.errors.IndexOutOfBounds;
import spartan.errors.InvalidArgument;
import spartan.runtime.VirtualMachine;

public final class Vector implements Datum, Callable
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
    
  @Override
  public Type type()
  {
    return Type.VECTOR;
  }
  
  @Override
  public String repr()
  {
    return elems.stream()
      .map(e -> e.repr())
      .collect(Collectors.joining(" ", "[", "]"));
  }  
  
  @Override
  public void apply(VirtualMachine vm)
  {
    if (!vm.peekArg().type().isInt())
      throw new InvalidArgument();
    int index = ((Integral)vm.popArg()).intValue();
    vm.result = get(index);
    vm.popFrame();
  }
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return numArgs == 1;
  }
  
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

  public boolean eq(Vector other, BiPredicate<Datum, Datum> eq)
  {
    if (this.length() != other.length())
      return false;
    
    var n = this.elems.size();
    for (int i = 0; i < n; ++i)
      if (!eq.test(this.elems.get(i), other.elems.get(i)))
        return false;
    
    return true;
  }
    
  private static final int DEFAULT_INITIAL_CAPACITY = 8;
  private final ArrayList<Datum> elems;
}
