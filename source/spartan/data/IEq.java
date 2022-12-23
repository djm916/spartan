package spartan.data;

public interface IEq<T> {
  boolean isEqual(Datum other);
  boolean isEqual(T other);
}
