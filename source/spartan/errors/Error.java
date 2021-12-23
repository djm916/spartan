package spartan.errors;

public class Error extends Exception
{
  public Error(String message)
  {
    super(message);
  }
  
  // Do not generate a stack trace  
  public Throwable fillInStackTrace()
  {
    return this;
  }  
}
