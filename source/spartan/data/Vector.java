package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.function.BiPredicate;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class Vector extends Callable
{
  public static Vector fromList(List elems)
  {
    Vector result = new Vector(elems.length());
    for (int i = 0; elems != List.Empty; ++i, elems = elems.cdr())
      result.elems[i] = elems.car();
    return result;
  }
  
  public static Vector create(Int numElems, Datum init)
  {
    var n = numElems.intValue();
    var result = new Vector(n);
    for (int i = 0; i < n; ++i)
      result.elems[i] = init;
    return result;
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
    return Stream.of(elems)
      .map(e -> e.repr())
      .collect(Collectors.joining(" ", "[", "]"));
  }
  
  public int length()
  {
    return elems.length;
  }
  
  public Datum get(Int index) throws NoSuchElement
  {
    try {
      return elems[index.intValue()];
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void set(Int index, Datum value) throws NoSuchElement
  {
    try {
      elems[index.intValue()] = value;
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public static boolean eq(Vector x, Vector y, BiPredicate<Datum, Datum> eq)
  {
    if (x.length() != y.length())
      return false;
    
    for (int i = 0; i < x.length(); ++i)
      if (!eq.test(x.elems[i], y.elems[i]))
        return false;
    
    return true;
  }
  
  public void apply(VirtualMachine vm) throws NoSuchElement, TypeMismatch
  {
    if (vm.peekArg().type() != Type.Int)
      throw new TypeMismatch();
    var index = (Int) vm.popArg();
    vm.result = get(index);
    vm.popFrame();
  }
  
  private Vector(int numElems)
  {
    super(1, false);
    this.elems = new Datum[numElems];
  }
  
  private Vector(Vector that)
  { 
    super(1, false);  
    this.elems = new Datum[that.elems.length];
    System.arraycopy(that.elems, 0, this.elems, 0, elems.length);
  }
  
  private final Datum[] elems;
}
