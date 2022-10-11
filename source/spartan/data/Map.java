package spartan.data;

//import java.util.Map;
import java.util.IdentityHashMap;
import java.util.stream.Collectors;
import spartan.errors.InvalidArgument;
import spartan.errors.WrongNumberArgs;

public final class Map extends Datum
{
  public static Map fromList(List elems)
  {
    int numElems = elems.length();
    if (numElems % 2 != 0)
      throw new WrongNumberArgs();
    var result = new Map(numElems);
    for (; !elems.empty(); elems = elems.cdr()) {
      var key = elems.car();
      elems = elems.cdr();
      var val = elems.car();
      result.put(key, val);
    }
    return result;
  }
  
  @Override
  public Type type()
  {
    return Type.MAP;
  }
  
  @Override
  public String repr()
  {
    return elems.entrySet().stream()      
      .map(e -> e.getKey().repr() + " " + e.getValue().repr())
      .collect(Collectors.joining(" ", "{", "}"));
  }
  
  public void put(Datum key, Datum value)
  {
    if (key.type() != Type.SYMBOL)
      throw new InvalidArgument();
    elems.put((Symbol)key, value);
  }
  
  public Map()
  {
    this(DEFAULT_CAPACITY);
  }
  
  public Map(int capacity)
  {
    this.elems = new IdentityHashMap<>(capacity);
  }
  
  private static final int DEFAULT_CAPACITY = 8;
  private final java.util.Map<Symbol, Datum> elems;
}
