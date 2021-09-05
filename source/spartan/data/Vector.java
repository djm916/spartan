package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Core;

public class Vector extends Callable
{
  public static Vector fromList(List elems)
  {
    Vector result = new Vector(elems.length());
    for (int i = 0; elems != List.Empty; ++i, elems = elems.cdr())
      result.elems[i] = elems.car();
    return result;
  }
  
  public final Type type()
  {
    return Type.Vector;
  }
  
  public final String repr()
  {
    return Stream.of(elems)
      .map(e -> e.repr())
      .collect(Collectors.joining(" ", "[", "]"));
  }
  
  public final int length()
  {
    return elems.length;
  }
  
  public final Datum at(int index) throws NoSuchElement
  {
    if (index < 0 || index >= elems.length)
      throw new NoSuchElement();
    return elems[index];
  }
  
  public final boolean eq(Vector that) throws TypeMismatch
  {
    if (this.length() != that.length())
      return false;
    
    for (int i = 0; i < this.length(); ++i)
      if (!Core.eq(this.elems[i], that.elems[i]))
        return false;
    
    return true;
  }
  
  public void apply(VirtualMachine vm) throws Error
  {
    if (vm.peekArg().type() != Type.Int)
      throw new TypeMismatch();
    vm.result = at(((Int)vm.popArg()).value);
    vm.popFrame();
  }
  
  private Vector(int numElems)
  {
    super(1, false);
    this.elems = new Datum[numElems];
  }
  
  private final Datum[] elems;
}
