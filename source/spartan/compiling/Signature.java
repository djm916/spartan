package spartan.compiling;

/**
  Represents the basic data of a procedure's signature, including
  the number of required arguments, and whether or not the procedure
  is variadic.
*/
public record Signature(int requiredArgs, boolean isVariadic)
{
  public boolean matches(int numArgs)
  {
    return isVariadic ? numArgs >= requiredArgs : numArgs == requiredArgs;
  }
}
