package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.builtins.CoreLib;

public final class Vector extends Datum
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
  
  public Datum get(Int index) throws NoSuchElement
  {
    try {
      return elems[index.value()];
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void set(Int index, Datum value) throws NoSuchElement
  {
    try {
      elems[index.value()] = value;
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public boolean eq(Vector that)
  {
    if (this.length() != that.length())
      return false;
    
    for (int i = 0; i < this.length(); ++i)
      if (!CoreLib.eq(this.elems[i], that.elems[i]))
        return false;
    
    return true;
  }
  
  public Vector copy()
  {
    return new Vector(this);
  }
    
  public static Vector create(Int numElems, Datum init)
  {
    var n = numElems.value();
    var result = new Vector(n);
    for (int i = 0; i < n; ++i)
      result.elems[i] = init;
    return result;
  }
  
  private Vector(int numElems)
  {
    this.elems = new Datum[numElems];
  }
  
  private Vector(Vector that)
  {
    this.elems = new Datum[that.elems.length];
    System.arraycopy(that.elems, 0, this.elems, 0, elems.length);
  }
  
  private final Datum[] elems;
}
