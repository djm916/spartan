package spartan.data;

import java.util.Set;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Builtins;
import spartan.data.Symbol;

public class Record extends Datum
{
  public static Record fromList(List elems) throws TypeMismatch
  {
    Record result = new Record();
    for (; elems != List.Empty; elems = elems.rest) {
      if (elems.first.type() != Type.Symbol)
        throw new TypeMismatch();
      Symbol key = (Symbol)elems.first;
      if (elems.rest == List.Empty)
        throw new TypeMismatch();
      Datum value = elems.rest.first;
      elems = elems.rest;
      result.members.put(key, value);
    }
    return result;
  }
  
  public final Type type()
  {
    return Type.Record;
  }
  
  public final String repr()
  {
    return members.entrySet().stream()
      .map(e -> String.format("%s %s", e.getKey().repr(), e.getValue().repr()))
      .collect(Collectors.joining(" ", "{", "}"));
  }
  
  public final Datum at(Symbol key) throws NoSuchElement
  {
    Datum value = members.get(key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }

  public final int size()
  {
    return members.size();
  }
  
  public boolean eq(Record that) throws TypeMismatch
  {
    if (this.size() != that.size())
      return false;
        
    for (Symbol key : this.members.keySet()) {
      if (!that.members.containsKey(key))
        return false;
      if (!Builtins.eq(this.members.get(key), that.members.get(key)))
        return false;
    }
    
    return true;
  }

  private Record()
  {
    members = new IdentityHashMap<>();
  }
  
  private final Map<Symbol, Datum> members;
}
