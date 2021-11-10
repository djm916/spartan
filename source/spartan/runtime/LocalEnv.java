package spartan.runtime;

import spartan.data.Datum;
import spartan.data.Nil;

public final class LocalEnv
{
  final Datum[] slots;
  final LocalEnv parent;
  
  LocalEnv(int numSlots, LocalEnv parent)
  {
    this.slots = new Datum[numSlots];  
    for (int i = 0; i < numSlots; ++i)
      this.slots[i] = Nil.Instance;
    this.parent = parent;
  }
  
  final Datum load(int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    return self.slots[offset];
  }
  
  final void store(Datum x, int depth, int offset)
  {
    LocalEnv self = this;
    for (; depth > 0; --depth)
      self = self.parent;
    self.slots[offset] = x;
  }
}
