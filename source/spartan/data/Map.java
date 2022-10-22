package spartan.data;

import java.util.IdentityHashMap;
import java.util.stream.Collectors;
import spartan.errors.InvalidArgument;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public final class Map implements Datum, Callable
{
  public static Map fromList(List map)
  {
    int numElems = map.length();
    if (numElems % 2 != 0)
      throw new WrongNumberArgs();
    var result = new Map(numElems);
    for (; !map.empty(); map = map.cdr()) {
      var key = map.car();
      map = map.cdr();
      var val = map.car();
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
    return map.entrySet().stream()      
      .map(e -> e.getKey().repr() + " " + e.getValue().repr())
      .collect(Collectors.joining(" ", "{", "}"));
  }
  
  @Override
  public void apply(VirtualMachine vm)
  {
    vm.result = get(vm.popArg());
    vm.popFrame();
  }
  
  @Override
  public boolean arityMatches(int numArgs)
  {
    return numArgs == 1;
  }
  
  public void put(Datum key, Datum value)
  {
    if (key.type() != Type.SYMBOL)
      throw new InvalidArgument();
    map.put((Symbol)key, value);
  }
  
  public Datum get(Datum key)
  {
    if (key.type() != Type.SYMBOL)
      throw new InvalidArgument();
    var value = map.get((Symbol)key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }
  
  public Map()
  {
    this(DEFAULT_CAPACITY);
  }
  
  public Map(int capacity)
  {
    this.map = new IdentityHashMap<>(capacity);
  }
  
  private static final int DEFAULT_CAPACITY = 8;
  private final java.util.Map<Symbol, Datum> map;
}
