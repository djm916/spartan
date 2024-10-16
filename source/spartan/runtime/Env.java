package spartan.runtime;

import spartan.data.Datum;

public record Env(Datum[] slots, Env parent)
{
  public Env(int numSlots, Env parent)
  {
    this(new Datum[numSlots], parent);
  }
  
  public Datum get(int index)
  {
    return slots[index];
  }
  
  public void set(int index, Datum value)
  {
    slots[index] = value;
  }
}
