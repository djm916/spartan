package spartan.data;

import java.util.stream.Collectors;

/**
 * An instance of a record type.
 *
 */
public final class Record implements Datum
{
  /**
   * Constructor
   *
   * @param type The record's type
   * @param values The initial values for the record's fields
   */
  public Record(RecordDescriptor rtd, Datum... values)
  {
    this.rtd = rtd;
    this.slots = values;
  }
  
  @Override // Datum
  public Type type()
  {
    return rtd.instanceType();
  }
  
  /**
   * Gets the value of the field assigned to the given slot
   *
   * @param slot the field slot index
   */
  public Datum get(int slot)
  {
    return slots[slot];
  }
  
  /**
   * Sets the value of the field assigned to the given slot
   *
   * @param slot the field slot index
   * @param value the new field value
   */
  public void set(int slot, Datum value)
  {
    slots[slot] = value;
  }
  
  public Datum[] fieldValues()
  {
    return slots;
  }
  
  private final RecordDescriptor rtd;
  private final Datum[] slots;
}
