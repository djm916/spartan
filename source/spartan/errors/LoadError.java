package spartan.errors;

public class LoadError extends IOError
{
  public LoadError(String fileName)
  {
    super("unable to load file \"" + fileName + "\"");
  }
}
