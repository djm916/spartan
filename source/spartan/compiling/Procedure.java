package spartan.compiling;

import spartan.runtime.Inst;

public final class Procedure
{
  final Inst code;
  final int requiredArgs;
  final boolean isVariadic;
  
  Procedure(Inst code, int requiredArgs, boolean isVariadic)
  {
    this.code = code;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
}
