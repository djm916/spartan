package spartan.data;

/**
 * Interface for "associative" data types (mappings, lists, vectors, etc.)
 */
public interface IAssoc
{
  Datum get(Datum key);
  void set(Datum key, Datum value);
}
