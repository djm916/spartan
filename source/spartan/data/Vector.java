package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.function.BiPredicate;
import java.util.ArrayList;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.errors.IntegerOverflow;
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
  
  public static Vector create(Int length, Datum init) throws IntegerOverflow
  {
    return create(length.intValue(), init);
  }
  
  public Vector(int length)
  {
    super(1, false);
    elems = new ArrayList<Datum>(length);
  }
  
  public Vector(Int length) throws IntegerOverflow
  {
    this(length.intValue());
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
  
  public Datum get(Int index) throws NoSuchElement, IntegerOverflow
  {
    return get(index.intValue());
  }
  
  private Datum get(int index) throws NoSuchElement
  {
    try {
      return elems.get(index);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void set(int index, Datum value) throws NoSuchElement
  {
    try {
      elems.set(index, value);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void set(Int index, Datum value) throws NoSuchElement, IntegerOverflow
  {
    set(index.intValue(), value);
  }
  
  public void append(Datum x)
  {
    elems.add(x);
  }
  
  public static boolean eq(Vector x, Vector y, BiPredicate<Datum, Datum> eq)
  {
    if (x.length() != y.length())
      return false;
    
    var n = x.length();
    for (int i = 0; i < n; ++i)
      if (!eq.test(x.elems.get(i), y.elems.get(i)))
        return false;
    
    return true;
  }
  
  public void apply(VirtualMachine vm) throws NoSuchElement, TypeMismatch, IntegerOverflow
  {
    if (vm.peekArg().type() != Type.Int)
      throw new TypeMismatch();
    var index = ((Int) vm.popArg()).intValue();
    vm.result = get(index);
    vm.popFrame();
  }
  
  private final ArrayList<Datum> elems;
}
