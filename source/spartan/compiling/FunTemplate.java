package spartan.compiling;

class FunTemplate
{
  final int numSlots;
  final int requiredArgs;
  final boolean isVariadic;
  
  FunTemplate(int numSlots, int requiredArgs, boolean isVariadic)
  {
    this.numSlots = numSlots;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
}
