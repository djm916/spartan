package spartan.parsing;

/**
 * Records a source code position (input source, line, and column number).
 */
public record Position(String source, int line, int column)
{
  public String toString()
  {
    return String.format("%s: %d: %d", source, line, column);
  }
}
