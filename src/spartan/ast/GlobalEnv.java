package spartan.ast;

import spartan.errors.MultipleDefinition;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

class GlobalEnv
{
  private static class Slot implements Variable
  {
    final String id;
    final int depth;
    boolean hasValue;
    
    Slot(String id, int depth, boolean hasValue)
    {
      this.id = id;
      this.depth = depth;
      this.hasValue = hasValue;
    }
        
    public String id()
    {
      return id;
    }

    public boolean global()
    {
      return true;
    }

    public int depth()
    {
      return depth;
    }

    public boolean hasValue()
    {
      return hasValue;
    }

    public void setValue()
    {
      hasValue = true;
    }
  }
  
  private final Map<String, Slot> slots = new HashMap<>();
  
  Optional<Variable> lookup(String id)
  {
    if (!slots.containsKey(id))
      return Optional.empty();
    else
      return Optional.of(slots.get(id));
  }
  
  void bind(String id) throws MultipleDefinition
  {
    if (slots.containsKey(id))
      throw new MultipleDefinition(id);
    slots.put(id, new Slot(id, slots.size(), false));
  }
}
