package spartan.data;

/**
 * A procedure signature (number of formal parameters)
 *
 * @param min minimum number of arguments accepted
 * @param max maximum number of arguments accepted
 */
public record Signature(int min, int max)
{
  /**
   * Returns a signature for a procedure that accepts exactly n arguments.
   */
  public static Signature fixed(int n)
  {
    return new Signature(n, n);
  }
  
  /**
   * Returns a signature for a procedure that accepts at least n arguments,
   * and an arbitrary number of additional arguments.
   */
  public static Signature variadic(int n)
  {
    return new Signature(n, Integer.MAX_VALUE);
  }
  
  /**
   * Returns a signature for a procedure that accepts at least n arguments,
   * and up to m additional optional arguments.
   */
  public static Signature variadic(int n, int m)
  {
    return new Signature(n, n + m);
  }
  
  /**
   * Check if this signature accepts n arguments.
   */
  public boolean matches(int n)
  {
    return min <= n && n <= max;
  }
}
