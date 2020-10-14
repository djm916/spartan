package spartan.ast;

import java.util.Map;
import java.util.HashMap;

class GlobalEnv
{
  private final Map<String, Index> env = new HashMap<>();
  
  Index lookup(String id)
  {
    return env.get(id);
  }
  
  void bind(String id)
  {
    env.put(id, new Index(env.size(), true));
  }
  
  void set(String id)
  {
    env.get(id).set();
  }
  
  boolean isSet(String id)
  {
    return env.get(id).isSet();
  }
}
