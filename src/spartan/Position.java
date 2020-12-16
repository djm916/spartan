package spartan;

public class Position
{
  public final String file;
  public final int line;
  public final int column;
  
  public Position(String file, int line, int column)
  {
    this.file = file;
    this.line = line;
    this.column = column;
  }
}
