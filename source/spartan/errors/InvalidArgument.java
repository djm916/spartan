package spartan.errors;

public class InvalidArgument extends Error
{
  public InvalidArgument()
  {
    super("invalid argument");
  }
  
  public InvalidArgument(String message)
  {
    super("invalid argument: " + message);
  }
}
