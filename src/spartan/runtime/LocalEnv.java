package spartan.runtime;

import spartan.data.Datum;

public final class LocalEnv
{
  final Datum[] slots;
  final LocalEnv parent;
  
  public LocalEnv(int numSlots, LocalEnv parent)
  {
    this.slots = new Datum[numSlots];
    this.parent = parent;
  }
  
  public final Datum load(int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    return self.slots[offset];
  }
  
  public final void store(Datum x, int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    self.slots[offset] = x;
  }
}
