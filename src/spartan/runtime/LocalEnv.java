package spartan.runtime;

import spartan.data.Value;

public class LocalEnv
{
  Value[] bindings;
  LocalEnv parent;
  
  public LocalEnv(int numBindings, LocalEnv parent)
  {
    this.bindings = new Value[numBindings];
    this.parent = parent;
  }
  
  public Value load(int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    return self.bindings[offset];
  }
  
  public void store(Value value, int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    self.bindings[offset] = value;
  }
}
