package spartan.compiling;

import spartan.runtime.Inst;

class Macro
{
  final Inst code;
  final int requiredArgs;
  final boolean isVariadic;
  
  Macro(Inst code, int requiredArgs, boolean isVariadic)
  {
    this.code = code;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
}
