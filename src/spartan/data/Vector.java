package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;

public class Vector extends Datum
{
  public Vector(Datum ... elems)
  {
    this.elems = elems;
  }
  
  public final Type type()
  {
    return Type.Vector;
  }
  
  public final String repr()
  {
    return Stream.of(elems)
      .map(e -> e.repr())
      .collect(Collectors.joining(", ", "(", ")"));
  }
  
  public final int size()
  {
    return elems.length;
  }
  
  public final Datum at(int index) throws NoSuchElement
  {
    try {
      return elems[index];
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
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

  private final Datum[] elems;
}
