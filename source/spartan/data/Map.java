package spartan.data;

import java.util.stream.Collectors;
import spartan.errors.InvalidArgument;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public final class Map implements Datum, Callable
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
  public String type()
  {
    return "map";
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
    map.put(key, value);
  }
  
  public Datum get(Datum key)
  {
    var value = map.get(key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }
    
  public int length()
  {
    return map.size();
  }
  
  public List keys()
  {
    return List.of(map.keySet());
  }
  
  public List values()
  {
    return List.of(map.values());
  }
  
  public List entries()
  {
    var builder = new List.Builder();
    map.entrySet().forEach((e) -> builder.add(List.of(e.getKey(), e.getValue())));
    return builder.build();
  }
  
  public Map()
  {
    this(DEFAULT_CAPACITY);
  }
  
  public Map(int capacity)
  {
    this.map = new java.util.HashMap<>(capacity);
  }
  
  private static final int DEFAULT_CAPACITY = 8;
  private final java.util.Map<Datum, Datum> map;
}
