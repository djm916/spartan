package spartan.runtime;

import spartan.data.Value;
import java.util.List;
import java.util.ArrayList;

class GlobalEnv
{
  private final List<Value> values;
  
  GlobalEnv(int initialCapacity)
  {
    values = new ArrayList<>(initialCapacity);
  }
  
  Value load(int depth)
  {
    return values.get(depth);
  }
  
  void store(Value value, int depth)
  {
    if (depth >= values.size())
      values.add(value);
    else
      values.set(depth, value);
  }
}
