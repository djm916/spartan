package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;

public class Vector extends Datum
{
  public static Vector fromList(List elems)
  {
    Vector result = new Vector(elems.length());
    for (int i = 0; elems != List.Empty; ++i, elems = elems.rest)
      result.elems[i] = elems.first;
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
  
  public final int size()
  {
    return elems.length;
  }
  
  public final Datum at(int index) throws NoSuchElement
  {
    if (index >= elems.length)
      throw new NoSuchElement();
    return elems[index];
  }
  
  public final boolean eq(Vector that) throws TypeMismatch
  {
    if (this.size() != that.size())
      return false;
    
    for (int i = 0; i < this.size(); ++i)
      if (!Builtins.eq(this.elems[i], that.elems[i]))
        return false;
    
    return true;
  }
  
  private Vector(int numElems)
  {
    this.elems = new Datum[numElems];
  }
  
  private final Datum[] elems;
}
