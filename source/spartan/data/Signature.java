package spartan.data;

/**
  Represents the basic data of a procedure's signature, including
  the number of required arguments, and whether or not the procedure
  is variadic.
*/
public record Signature(int minArgs, int maxArgs)
{
  public static Signature fixed(int required)
  {
    return new Signature(required, required);
  }
  
  public static Signature variadic(int required)
  {
    return new Signature(required, Integer.MAX_VALUE);
  }
  
  public static Signature variadic(int required, int optional)
  {
    return new Signature(required, required + optional);
  }
  
  public boolean matches(int numArgs)
  {
    return minArgs <= numArgs && numArgs <= maxArgs;
  }
}
