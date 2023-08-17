package spartan.data;

/**
 * Interface for associative data types (list, vector, table, etc.)
 */
public interface IAssoc extends Datum
{
  /**
   * Lookup the value associated with the given key
   *
   * @param key Key to find
   * @returns The value associated with the key, or Nil.VALUE if none
   */
  Datum get(Datum key);
    
  void set(Datum key, Datum value);
}
