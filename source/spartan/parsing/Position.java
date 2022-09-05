package spartan.parsing;

public record Position(String source, int line, int column)
{
  public String toString()
  {
    return String.format("%s: %d: %d", source, line, column);
  }
}
