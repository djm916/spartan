package spartan.data;

/**
 * Interface for things that have a size or length
 */
public interface ILen extends Datum
{
  int length();
  
  default boolean isEmpty()
  {
    return length() == 0;
  }
}
