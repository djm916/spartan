package spartan.runtime;

import spartan.data.Value;
import java.util.Map;
import java.util.HashMap;

class GlobalEnv
{
  private final Map<String, Value> values = new HashMap<>();

  void bind(String id, Value value)
  {
    values.put(id, value);
  }
  
  Value lookup(String id)
  {
    return values.get(id);
  }
}
