package spartan;

public class Position
{
  public final String source;
  public final int line;
  public final int column;
  
  public Position(String source, int line, int column)
  {
    this.source = source;
    this.line = line;
    this.column = column;
  }
}
