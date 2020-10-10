package spartan.data;

import java.util.Set;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;

public class Record extends Value
{
  public Record(String[] labels, Value[] values)
  {
    members = new IdentityHashMap<>(labels.length);
    for (int i = 0; i < labels.length; ++i)
      members.put(labels[i], values[i]);
  }
  
  public Type type()
  {
    return Type.Record;
  }
  
  public String repr()
  {
    return members.entrySet().stream()
      .map(e -> String.format("%s: %s", e.getKey(), e.getValue().repr()))
      .collect(Collectors.joining(", ", "{", "}"));
  }
  
  public Value at(String label) throws NoSuchElement
  {
    Value result = members.get(label);
    if (result == null)
      throw new NoSuchElement();
    return result;
  }

  public int size()
  {
    return members.size();
  }
  
  public static boolean eq(Record x, Record y) throws TypeMismatch
  {
    if (x.size() != y.size())
      return false;
        
    for (String label : x.members.keySet()) {
      if (!y.members.containsKey(label))
        return false;
      if (!Builtins.eq(x.members.get(label), y.members.get(label)))
        return false;
    }
    
    return true;
  }

  private final Map<String, Value> members;
}
