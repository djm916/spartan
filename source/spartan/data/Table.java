package spartan.data;

import java.util.stream.Collectors;
import spartan.errors.InvalidArgument;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;

public final class Table implements Datum
{
  public static Table fromList(List elems)
  {
    var result = new Table();
    while (!elems.isEmpty()) {      
      var key = elems.car();
      elems = elems.cdr();
      if (elems.isEmpty())
        throw new WrongNumberArgs();
      var val = elems.car();
      result.assoc(key, val);
      elems = elems.cdr();
    }
    return result;
  }
  
  public Table()
  {
    this(DEFAULT_CAPACITY);
  }
  
  public Table(int capacity)
  {
    this.map = new java.util.HashMap<>(capacity);
  }
  
  @Override // Datum
  public String type()
  {
    return "table";
  }
  
  @Override // Datum
  public String repr()
  {
    return map.entrySet().stream()      
      .map(e -> e.getKey().repr() + " " + e.getValue().repr())
      .collect(Collectors.joining(" ", "{", "}"));
  }
  
  public Datum find(Datum key)
  {
    var value = map.get(key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }
  
  public Datum findOrElse(Datum key, Datum theDefault)
  {
    var value = map.get(key);
    return value == null ? theDefault : value;
  }
  
  public boolean contains(Datum key)
  {
    return map.get(key) == null;
  }
  
  public void assoc(Datum key, Datum value)
  {
    map.put(key, value);
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
  
  private static final int DEFAULT_CAPACITY = 8;
  private final java.util.Map<Datum, Datum> map;
}
