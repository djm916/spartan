package spartan.data;

import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;

public class Tuple extends Value
{
  public Tuple(Value ... elems)
  {
    this.elems = elems;
  }
  
  public Type type()
  {
    return Type.Tuple;
  }
  
  public String repr()
  {
    return Stream.of(elems)
      .map(e -> e.repr())
      .collect(Collectors.joining(", ", "(", ")"));
  }
  
  public int size()
  {
    return elems.length;
  }
  
  public Value at(int index) throws NoSuchElement
  {
    try {
      return elems[index];
    }
    catch (ArrayIndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public static boolean eq(Tuple x, Tuple y) throws TypeMismatch
  {
    if (x.size() != y.size())
      return false;
    
    for (int i = 0; i < x.size(); ++i)
      if (!Builtins.eq(x.elems[i], y.elems[i]))
        return false;
    
    return true;
  }

  private final Value[] elems;
}
