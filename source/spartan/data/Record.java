package spartan.data;

import java.util.stream.Collectors;

/**
 * An instance of a record type.
 *
 * @param rtd The record type descriptor for this record
 * @param fields The values of the fields of this record
 */
public record Record(RecordDescriptor rtd, Datum[] fields) implements Datum
{
  /**
   * Constructor
   *
   * @param rtd A record type descriptor
   * @param values The initial values for the record's fields
   */
  @Override // Datum
  public Type type()
  {
    return rtd.instanceType();
  }
  
  /**
   * Gets the value of the field assigned to the given slot
   *
   * @param index the field index
   */
  public Datum get(int index)
  {
    return fields[index];
  }
  
  /**
   * Sets the value of the field assigned to the given slot
   *
   * @param index the field index
   * @param value the new field value
   */
  public void set(int index, Datum value)
  {
    fields[index] = value;
  }
}
