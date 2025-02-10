package spartan.data;

/**
 * An instance of a record type.
 *
 * @param rtd the record type descriptor for this record
 * @param fields the values of the fields of this record
 */
public record Record(RecordDescriptor rtd, Datum[] fields) implements Datum
{
  @Override // Datum
  public Type type()
  {
    return rtd.instanceType();
  }
  
  /**
   * Gets the field value at the given index
   *
   * @param index the field index
   * @return the field value
   */
  public Datum get(int index)
  {
    return fields[index];
  }
  
  /**
   * Sets the field value at the given index
   *
   * @param index the field index
   * @param value the new field value
   */
  public void set(int index, Datum value)
  {
    fields[index] = value;
  }
}
