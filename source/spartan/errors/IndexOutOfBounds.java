package spartan.errors;

public class IndexOutOfBounds extends Error
{
  public IndexOutOfBounds()
  {
    super("index out of bounds");
  }
  
  public IndexOutOfBounds(String message)
  {
    super("index out of bounds" + message);
  }
}
