package spartan.compiling;

class FunInfo
{
  public final int requiredArgs;
  public final boolean isVariadic;
  
  FunInfo(int requiredArgs, boolean isVariadic)
  {
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
}
