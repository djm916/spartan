package spartan.data;

public interface Sequence
{
  Sequence conj(Datum x);
  Datum first();
  Sequence rest();
  Datum nth(int index);
  int length();
  boolean empty();
}
