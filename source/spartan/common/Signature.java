package spartan.common;

public record Signature(int requiredArgs, boolean isVariadic)
{
  public boolean matches(int numArgs)
  {
    return !isVariadic ? numArgs == requiredArgs
                       : numArgs >= requiredArgs;
  }
}
