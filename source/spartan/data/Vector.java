package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Core;

public final class Vector extends Callable
{
  public static Vector fromList(List elems)
  {
    Vector result = new Vector(elems.length());
    for (int i = 0; elems != List.Empty; ++i, elems = elems.cdr())
      result.elems[i] = elems.car();
    return result;
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
  
  public Datum at(int index) throws NoSuchElement
  {
    return elems[index];
  }
  
  public boolean eq(Vector that) throws TypeMismatch
  {
    if (this.length() != that.length())
      return false;
    
    for (int i = 0; i < this.length(); ++i)
      if (!Core.eq(this.elems[i], that.elems[i]))
        return false;
    
    return true;
  }
  
  public void apply(VirtualMachine vm) throws TypeMismatch, NoSuchElement
  {
    if (vm.peekArg().type() != Type.Int)
      throw new TypeMismatch();
    int index = ((Int)vm.popArg()).value;
    if (index < 0 || index >= elems.length)
      throw new NoSuchElement();
    vm.result = at(index);
    vm.popFrame();
  }
  
  private Vector(int numElems)
  {
    super(1, false);
    this.elems = new Datum[numElems];
  }
  
  private final Datum[] elems;
}
