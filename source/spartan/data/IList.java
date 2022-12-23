package spartan.data;

public interface IList
{
  IList conj(Datum x);
  Datum first();
  IList rest();
  Datum nth(int index);
  int length();
  boolean isEmpty();
}
