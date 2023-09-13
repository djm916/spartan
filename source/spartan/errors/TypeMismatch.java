package spartan.errors;

public class TypeMismatch extends Error
{
  private static final String MSG_FMT = "type mismatch";
  
  public TypeMismatch()
  {
    super(MSG_FMT);
  }
}
