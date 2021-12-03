package spartan.compiling;

import spartan.runtime.Inst;

public final class ProcTemplate
{
  public final Inst code;
  public final int requiredArgs;
  public final boolean isVariadic;
  
  ProcTemplate(Inst code, int requiredArgs, boolean isVariadic)
  {
    this.code = code;
    this.requiredArgs = requiredArgs;
    this.isVariadic = isVariadic;
  }
}
