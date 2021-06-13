package spartan.errors;

public class Error extends Exception
{
  public Error(String message)
  {
    super(message);
  }
  
  public Throwable fillInStackTrace()
  {
    // Do not generate a stack trace
    return this;
  }
  
  public String toString()
  {
    return String.format("error: %s", getMessage());
  }
}
