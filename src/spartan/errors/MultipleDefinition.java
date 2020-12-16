package spartan.errors;

public class MultipleDefinition extends Error
{
  public MultipleDefinition(String id)
  {
    super("multiple definition of variable \"" + id + "\"");
  }
}
