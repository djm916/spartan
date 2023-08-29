package spartan.errors;

public class NoSuchPackage extends Error
{
  public NoSuchPackage(String pkg)
  {
    super("no such package " + pkg);
  }
}
