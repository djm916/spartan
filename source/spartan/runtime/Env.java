package spartan.runtime;

import spartan.data.Datum;

public record Env(Datum[] slots, Env parent)
{
  Env(int numSlots, Env parent)
  {
    this(new Datum[numSlots], parent);
  }
  
  Datum get(int index)
  {
    return slots[index];
  }
  
  void set(int index, Datum value)
  {
    slots[index] = value;
  }
}
