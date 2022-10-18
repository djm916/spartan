package spartan.parsing;

/**
  Records the position (input source, line, and column number)
  of expressions returned by the reader.
*/
public record Position(String source, int line, int column)
{
  public String toString()
  {
    return String.format("%s: %d: %d", source, line, column);
  }
}
