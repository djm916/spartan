package spartan.data;

import java.util.Set;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;

public class Record extends Datum
{
  public Record(String[] labels, Datum[] values)
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
  
  public Datum at(String label) throws NoSuchElement
  {
    Datum result = members.get(label);
    if (result == null)
      throw new NoSuchElement();
    return result;
  }

  public int size()
  {
    return members.size();
  }
  
  public boolean eq(Record that) throws TypeMismatch
  {
    if (this.size() != that.size())
      return false;
        
    for (String label : this.members.keySet()) {
      if (!that.members.containsKey(label))
        return false;
      if (!Builtins.eq(this.members.get(label), that.members.get(label)))
        return false;
    }
    
    return true;
  }

  private final Map<String, Datum> members;
}
