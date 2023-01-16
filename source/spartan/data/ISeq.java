package spartan.data;

public sealed interface ISeq permits List, Vector, Text
{
  Datum at(int index);
}
