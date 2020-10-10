package spartan.ast;

import java.util.List;
import java.util.ArrayList;

class GlobalEnv
{
  private final List<String> values = new ArrayList<>();
  
  Index lookup(String id)
  {
    int depth = values.indexOf(id);
    return depth < 0 ? null : new Index(depth, true);
  }
  
  void bind(String id)
  {
    values.add(id);
  }
}
