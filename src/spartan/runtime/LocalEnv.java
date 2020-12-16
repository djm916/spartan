package spartan.runtime;

import spartan.data.Value;

public class LocalEnv
{
  Value value;
  LocalEnv parent;
  
  public LocalEnv(Value value, LocalEnv parent)
  {
    this.value = value;
    this.parent = parent;
  }
  
  public Value load(int depth)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    return self.value;
  }
  
  public void store(Value value, int depth)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    self.value = value;
  }
}
