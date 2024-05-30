package spartan.data;

/**
  Represents a procedure's signature.
*/
public record Signature(int minArgs, int maxArgs)
{
  /**
   * Return a Signature for a procedure that accepts exactly n arguments
   */
  public static Signature fixed(int required)
  {
    return new Signature(required, required);
  }
  
  /**
   * Return a Signature for a procedure that accepts at least n arguments
   */
  public static Signature variadic(int required)
  {
    return new Signature(required, Integer.MAX_VALUE);
  }
  
  /**
   * Return a Signature for a procedure that accepts at least n arguments,
   * and at most n+m arguments
   */
  public static Signature variadic(int required, int optional)
  {
    return new Signature(required, required + optional);
  }
  
  public boolean matches(int numArgs)
  {
    return minArgs <= numArgs && numArgs <= maxArgs;
  }
}
