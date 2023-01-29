package spartan.data;

public interface IAssoc
{
  Datum get(Datum key);
  void set(Datum key, Datum value);
}
