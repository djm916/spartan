package spartan.data;

public interface ISeq
{
  boolean isEmpty();
  Datum get(int index);
  void set(int index, Datum elem);
}
