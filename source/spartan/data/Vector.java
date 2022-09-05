package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.function.BiPredicate;
import java.util.ArrayList;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class Vector extends Callable
{
  public static Vector fromList(List elems)
  {
    var result = new Vector(elems.length());
    for (; !elems.empty(); elems = elems.cdr())
      result.elems.add(elems.car());
    return result;
  }
  
  public static Vector create(int length, Datum init)
  {
    var result = new Vector(length);
    for (int i = 0; i < length; ++i)
      result.elems.add(init);
    return result;
  }
  
  public static Vector create(Int length, Datum init)
  {
    return create(length.value, init);
  }
  
  public Vector()
  {
    this(DefaultInitialSize);
  }
  
  public Vector(int length)
  {
    super(1, false);
    elems = new ArrayList<Datum>(length);
  }
  
  public Vector(Int length)
  {
    this(length.value);
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
  
  public Type type()
  {
    return Type.Vector;
  }
  
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
  
  public Datum get(Int index)
  {
    return get(index.value);
  }
  
  private Datum get(int index)
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
  
  public void set(Int index, Datum value)
  {
    set(index.value, value);
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
  
  public void apply(VirtualMachine vm)
  {
    if (vm.peekArg().type() != Type.Int)
      throw new TypeMismatch();
    var index = ((Int) vm.popArg()).value;
    vm.result = get(index);
    vm.popFrame();
  }
  
  private static final int DefaultInitialSize = 8;
  private final ArrayList<Datum> elems;
}
