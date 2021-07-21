package spartan.runtime;

import spartan.data.Datum;

public class LocalEnv
{
  final Datum[] slots;
  final LocalEnv parent;
  
  public LocalEnv(int numSlots, LocalEnv parent)
  {
    this.slots = new Datum[numSlots];
    this.parent = parent;
  }
  
  public Datum load(int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    return self.slots[offset];
  }
  
  public void store(Datum x, int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    self.slots[offset] = x;
  }
}
