package spartan.data;

import spartan.errors.IndexOutOfBounds;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.function.BiPredicate;
import java.util.ArrayList;

public final class Vector extends Datum
{
  public static Vector fromList(List elems)
  {
    var result = new Vector(elems.length());
    for (; !elems.empty(); elems = elems.cdr())
      result.append(elems.car());
    return result;
  }
  
  public static Vector create(int length, Datum init)
  {
    var result = new Vector(length);
    for (int i = 0; i < length; ++i)
      result.append(init);
    return result;
  }
  
  public Vector()
  {
    this(DEFAULT_INITIAL_SIZE);
  }
  
  public Vector(int initialCapacity)
  {
    elems = new ArrayList<>(initialCapacity);
  }
  
  public Vector(Vector that)
  { 
    this(that.length());
    elems.addAll(that.elems);
  }
  
  public Vector copy()
  {
    return new Vector(this);
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
  
  private static final int DEFAULT_INITIAL_SIZE = 8;
  private final ArrayList<Datum> elems;
}
