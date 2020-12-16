package spartan.ast;

import java.util.Deque;
import java.util.ArrayDeque;
import java.util.Optional;

class LocalEnv
{
  private static class Slot
  {
    final String id;
    boolean hasValue;
    
    Slot(String id, boolean hasValue)
    {
      this.id = id;
      this.hasValue = hasValue;
    }
  }
  
  private static class LocalVariable implements Variable
  {
    private final Slot slot;
    private final int depth;
    
    LocalVariable(Slot slot, int depth)
    {
      this.slot = slot;
      this.depth = depth;
    }
    
    public String id()
    {
      return slot.id;
    }

    public boolean global()
    {
      return false;
    }

    public int depth()
    {
      return depth;
    }

    public boolean hasValue()
    {
      return slot.hasValue;
    }

    public void setValue()
    {
      slot.hasValue = true;
    }
  }
  
  private final Deque<Slot> slots;
  
  LocalEnv()
  {
    this.slots = new ArrayDeque<>();
  }
  
  void bind(String id, boolean hasValue)
  {
    slots.push(new Slot(id, hasValue));
  }
  
  Optional<Variable> lookup(String id)
  {
    int depth = 0;
    for (Slot slot : slots) {
      if (id.equals(slot.id)) {
        return Optional.of(new LocalVariable(slot, depth));
      }
      ++depth;
    }
    return Optional.empty();
  }
  
  void remove(int n)
  {
    for (int i = 0; i < n; ++i)
      slots.pop();
  }
}
