package spartan.data;

public sealed interface ISize
permits List, Vector, Text
{
  int length();
}
